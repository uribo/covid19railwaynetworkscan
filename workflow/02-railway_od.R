####################################
# 鉄道ネットワーク
# 対象: 東京都内 JR東日本の路線
# sf_target_st ... 首都圏のJR各駅のポイント 529点、350駅
# 7都県 ... 東京、神奈川、埼玉、千葉、茨城、栃木、山梨
# 必要なデータ:
# - 地球地図日本
# - 国土数値情報 N02
# - 大都市交通センサス
####################################
source(here::here("workflow/01-data_download.R"))
library(sf)
library(dplyr)
library(stringr)
library(here)
library(ssrn)

make_adjacency_matrix_mod <- function(df_passenger_all,
                                      df_tgt_st_code001,
                                      df_tgt_st_code002,
                                      by, depart, arrive, distance) {
  d01 <-
    ssrn:::make_pass_volume(df_passenger_all,
                            df_tgt_st_code001,
                            by = by) %>%
    ssrn:::df_to_adjacency_distance({{ depart }}, {{ arrive }})
  d02 <-
    ssrn:::make_pass_volume(df_passenger_all,
                            df_tgt_st_code002,
                            by = by) %>%
    ssrn:::df_to_adjacency_distance({{ depart }}, {{ arrive }})
  d <-
    rbind(d01, d02) %>%
    distinct({{ depart }}, {{ arrive }}, .keep_all = TRUE)
  d %>%
    ssrn:::od_wider({{ depart }}, {{ arrive }}, {{ distance }}) %>%
    as.matrix()
}

make_passenger_matrix_mod <- function(df_passenger_all, df_tgt_st_code001, df_tgt_st_code002,
                                      by, depart, arrive, value) {
  d01 <-
    df_passenger_all %>%
    ssrn:::make_pass_volume(df_tgt_st_code001, by) %>%
    filter(!is.na({{ arrive }})) %>%
    group_by({{ depart }}, {{ arrive }}) %>%
    summarise(volume = sum(volume, na.rm = TRUE), .groups = "drop")
  d02 <-
    df_passenger_all %>%
    ssrn:::make_pass_volume(df_tgt_st_code002, by) %>%
    filter(!is.na({{ arrive }})) %>%
    group_by({{ depart }}, {{ arrive }}) %>%
    summarise(volume = sum(volume, na.rm = TRUE), .groups = "drop")
  d_tmp <-
    rbind(d01, d02)
  d <-
    df_passenger_all %>%
    group_by({{ depart }}, {{ arrive }}) %>%
    summarise(volume = sum(volume, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na({{ arrive }})) %>%
    right_join(tidyr::expand_grid(
      st_name = unique(c(unique(d_tmp$st_name), unique(d_tmp$next_st_name))),
      next_st_name = unique(c(unique(d_tmp$st_name), unique(d_tmp$next_st_name)))
    ),
    by = by)
  d %>%
    mutate({{ value }} := tidyr::replace_na({{ value }}, 0)) %>%
    ssrn:::od_wider({{ depart }}, {{ arrive }}, {{ value }}) %>%
    as.matrix()
}

if (!dir.exists(here("data")))
  dir.create(here("data"))
plan_tokyo_mesh <-
  drake::drake_plan(
    # Natural Earthのものだと境界が曖昧なので地球地図のポリゴンデータを使う
    sf_kanto =
      sf::st_read(here("data-raw/gm-japan/gm-jpn-all_u_2_2/polbnda_jpn.shp")) %>%
      sf::st_transform(crs = 4326) %>%
      # 群馬県は入らない
      dplyr::filter(nam %in% c("Tochigi Ken", "Ibaraki Ken",
                               "Saitama Ken", "Kanagawa Ken",
                               "Tokyo To", "Chiba Ken", "Yamanashi Ken")) %>%
      dplyr::filter(adm_code %in% c("08203", "08204", "08205",
                                    "08207", "08208", "08210",
                                    "08211", "08217", "08219", "08220", "08224",
                                    "08227", "08228", "08229", "08230",
                                    "08235",
                                    "08442", "08443", "08447", "08521", "08542", "08546",
                                    "08564",
                                    "09203", "09204", "09208", "09364",
                                    "11100", "11201", "11202", "11203", "11206", "11207",
                                    "11208", "11209", "11210", "11211", "11212", "11214",
                                    "11215", "11216", "11217", "11218", "11219", "11221",
                                    "11222", "11223", "11224", "11225", "11227", "11228",
                                    "11229", "11230", "11231", "11232", "11233", "11234",
                                    "11235", "11237", "11238", "11239", "11240", "11241",
                                    "11242", "11243", "11245", "11301",
                                    "11324", "11326", "11327", "11341", "11342", "11343", "11346",
                                    "11347", "11348", "11349", "11361", "11369", "11381",
                                    "11385", "11408", "11442", "11445", "11464", "11465",
                                    "12100", "12203", "12204", "12206", "12207", "12208",
                                    "12210", "12211", "12212", "12213", "12216", "12217", "12219",
                                    "12220", "12221", "12222", "12224", "12225", "12227",
                                    "12228", "12229", "12230", "12231", "12232", "12233",
                                    "12236", "12237", "12322", "12329", "12342", "12402",
                                    "12423", "12426", "12441",
                                    paste0(seq.int(13101, 13360)),
                                    "14100", "14130", "14150", "14201", "14203", "14204",
                                    "14205", "14206", "14207", "14208", "14210", "14211",
                                    "14212", "14213", "14214", "14215", "14216", "14217",
                                    "14218", "14301", "14321", "14341", "14342", "14361",
                                    "14362", "14363", "14366", "14382", "14382", "14383",
                                    "14384", "14401",
                                    "19206", "19212")) %>%
      verify(nrow(.) == 244L),
    sf_tokyo =
      sf_kanto %>%
      dplyr::filter(nam == "Tokyo To") %>%
      sf::st_combine() %>%
      sf::st_sf()
  )

# ナンバリング、乗降客数 --------------------------------------------------------------------
plan_mlit_census <-
  drake::drake_plan(
    df_station_code =
      # 鉄道駅コード
      readxl::read_xlsx("data-raw/mlit_transport12/001179689.xlsx") %>%
      assertr::verify(dim(.) == c(2052, 6)) %>%
      purrr::set_names(c("st_code",
                         "oc_name",
                         "rw_name",
                         "st_name",
                         "oc_code",
                         "rw_code")) %>%
      filter(oc_name == "東日本旅客鉄道" & stringr::str_detect(rw_name, "新幹線$", negate = TRUE)) %>%
      select(st_code, rw_name, st_name, rw_code) %>%
      mutate(rw_name = stringr::str_remove(rw_name, "（.+）")) %>%
      assertr::verify(nrow(.) == 640L) %>%
      filter(rw_code %in% c("001", "002", "003", "005", "006", "007", "008", "009", "010",
                            "011", "016", "017", "018", "019", "021", "022", "024",
                            "030",
                            "036", "037", "038", "040", "044", "045")) %>%
      assertr::verify(nrow(.) == 502L),
    df_passenger =
      # 線別駅間移動人員
      readxl::read_xlsx("data-raw/mlit_transport12/001179095.xlsx",
                        col_types = c("text", "text", "numeric", "text", "numeric", "numeric")) %>%
      assertr::verify(dim(.) == c(29963, 6)) %>%
      purrr::set_names(c("rw_code",
                         "departure_st_code", "departure_status",
                         "arrive_st_code", "arrive_status",
                         "volume")) %>%
      mutate(rw_code = stringr::str_pad(rw_code, width = 3, pad = "0"),
             across(ends_with("st_code"), ~stringr::str_pad(.x, width = 5, pad = "0"))) %>%
      # JR東日本の路線
      filter(rw_code %in% unique(df_station_code$rw_code)) %>%
      assertr::verify(dim(.) == c(11623, 6))
  )

# 鉄道のポイント -----------------------------------------------------------------
plan_st_geometry <-
  drake::drake_plan(
    ksj_railroadsection =
      sf::st_read(here("data-raw/ksj/N02-18_RailroadSection.geojson"),
                  as_tibble = TRUE, stringsAsFactors = FALSE) %>%
      purrr::set_names(dplyr::recode(names(.),
                                     N02_001 = "railwayType",
                                     N02_002 = "serviceProviderType", N02_003 = "railwayLineName",
                                     N02_004 = "operationCompany", N02_005 = "stationName")) %>%
      filter(railwayType == "11", serviceProviderType == "2") %>%
      select(-railwayType, -serviceProviderType) %>%
      # 他県の駅であっても東京を経由する路線上の駅は含める(この時点では東京を経由しなくてもある)
      st_join(st_sf(st_combine(sf_kanto)),
              left = FALSE) %>%
      verify(dim(.) == c(1263, 3)) %>%
      # 013 鶴見線, 012, 南武支線 --> 東京を経由しない
      filter(railwayLineName %in% c("横浜線", "京葉線", "五日市線",
                                    "東海道線", "八高線", "八高線",
                                    "東北線", "根岸線",
                                    "横須賀線",
                                    "高崎線", "山手線", "常磐線", "青梅線",
                                    "赤羽線（埼京線）", "総武線", "中央線",
                                    "南武線", "東北線（埼京線）", "武蔵野線")) %>%
      verify(nrow(.) == 975),
    ksj_railstation =
      sf::st_read(here("data-raw/ksj/N02-18_Station.geojson"),
                  as_tibble = TRUE, stringsAsFactors = FALSE) %>%
      purrr::set_names(dplyr::recode(names(.),
                                     N02_001 = "railwayType",
                                     N02_002 = "serviceProviderType", N02_003 = "railwayLineName",
                                     N02_004 = "operationCompany", N02_005 = "stationName")) %>%
      verify(dim(.) == c(10296, 6)) %>%
      # JR、在来線
      filter(railwayType == "11", serviceProviderType == "2") %>%
      select(-railwayType, -serviceProviderType) %>%
      st_join(st_sf(st_combine(sf_kanto)),
              left = FALSE) %>%
      verify(dim(.) == c(540, 4)) %>%
      # 2016年開業でセンサスデータにない（新駅）ので除外
      filter(stationName != "小田栄") %>%
      verify(nrow(.) == 539) %>%
      mutate(stationName = recode(stationName,
                              `四ツ谷` = "四ッ谷")) %>%
      filter(railwayLineName %in% unique(ksj_railroadsection$railwayLineName)) %>%
      verify(nrow(.) == 412L),
    d_grp =
      ksj_railstation %>%
      group_by(operationCompany, stationName),
    op_st_name =
      group_data(d_grp) %>%
      rowwise() %>%
      mutate(op_st_name = paste(operationCompany, stationName, sep = "_")) %>%
      ungroup() %>%
      purrr::pluck("op_st_name"),
    d_polygon =
      d_grp %>%
      group_map(
        ~ st_make_grid(.x, n = 1),
        .keep = FALSE
      ) %>%
      purrr::set_names(
        op_st_name) %>%
      ensurer::ensure(length(.) == 352L),
    d_area =
      d_polygon %>%
      purrr::map(~ units::set_units(st_area(.x), ha)),
    sf_target_st =
      d_polygon %>%
      purrr::map(~ st_centroid(.x)) %>%
      purrr::reduce(c) %>%
      st_as_sf() %>%
      purrr::update_list(op_st_name = op_st_name) %>%
      dplyr::rename(geometry = x) %>%
      dplyr::select(op_st_name, geometry) %>%
      tidyr::separate(col = op_st_name,
                      sep = "_",
                      into = c("op", "st_name")) %>%
      verify(nrow(.) == 352L) %>%
      inner_join(df_station_code,
                by = c("st_name")) %>%
      tibble::new_tibble(class = "sf", nrow = nrow(.)) %>%
      verify(nrow(.) == 501L),
    out_kanto_jr_st_rds =
      sf_target_st %>%
      readr::write_rds("data/kanto_jr_st.rds"))

# グラフの作成 ------------------------------------------------------------------
plan_st_graph <-
  drake::drake_plan(
    df_tgt_st_code001 =
      df_station_code %>%
      group_by(rw_name, rw_code) %>%
      transit_table(c(st_name, st_code)) %>%
      ungroup() %>%
      arrange(st_code) %>%
      mutate(next_st_name = if_else(is.na(next_st_name) & st_name == "田町" & rw_name == "山手線",
                                    "品川",
                                    next_st_name),
             next_st_code := if_else(is.na(next_st_code) & st_name == "田町" & rw_name == "山手線",
                                     "01001",
                                     str_pad(next_st_code, width = 5, pad = "0"))),
    df_tgt_st_code002 =
      df_station_code %>%
      group_by(rw_name, rw_code) %>%
      transit_table(c(st_name, st_code), reverse = TRUE) %>%
      ungroup() %>%
      arrange(st_code) %>%
      mutate(next_st_name = if_else(is.na(next_st_name) & st_name == "品川" & rw_name == "山手線",
                                    "田町",
                                    next_st_name),
             next_st_code = if_else(is.na(next_st_code) & st_name == "田町" & rw_name == "山手線",
                                    "01029",
                                    str_pad(next_st_code, width = 5, pad = "0"))),
    df_passenger_all =
      df_passenger %>%
      make_passenger_od(df_station_code,
                               depart = departure_st_code,
                               arrive = arrive_st_code,
                               location = st_code,
                        value = volume) %>%
      left_join(df_station_code %>%
                  select(arrive_st_code = st_code, next_st_name = st_name),
                by = "arrive_st_code") %>%
      select(departure_st_code, arrive_st_code, st_name, next_st_name, volume) %>%
      verify(dim(.) == c(6107, 5)),
    df_st_graph =
      df_passenger_all %>%
      distinct(departure_st_code, arrive_st_code, .keep_all = TRUE) %>%
      ssrn:::make_pass_volume(df_tgt_st_code001,
                              by = c("departure_st_code" = "st_code",
                                     "arrive_st_code" = "next_st_code",
                                     "st_name",
                                     "next_st_name")) %>%
      select(rw_code, rw_name, departure_st_code, st_name, arrive_st_code, next_st_name, volume) %>%
      verify(dim(.) == c(502, 7)),
    edge_list =
      df_st_graph %>%
      filter(!is.na(next_st_name)) %>%
      select(from = st_name,
             to = next_st_name,
             volume),
    node_list =
      tibble::tibble(
        station = unique(c(unique(edge_list$from),
                           unique(edge_list$to)))),
    routes_tidy =
      tidygraph::tbl_graph(nodes = node_list,
                           edges = edge_list,
                           directed = FALSE)
    )

# 駅間の隣接行列と乗降者数の行列データ --------------------------------------------------------------------
plan_adj_matrix <-
  drake::drake_plan(
    # 350 * 350
    kanto_jr_adjacent_matrix =
      make_adjacency_matrix_mod(df_passenger_all,
                                df_tgt_st_code001,
                                df_tgt_st_code002,
                                by = c("departure_st_code" = "st_code",
                                       "st_name",
                                       "next_st_name",
                                       "arrive_st_code" = "next_st_code"),
                                depart = st_name, arrive = next_st_name,
                                distance),
    out_kanto_adj_matrix_rds =
      kanto_jr_adjacent_matrix %>%
      readr::write_rds("data/kanto_jr_adjacent_matrix.rds"),
    kanto_jr_allpassernger_matrix =
       {
         1 / make_passenger_matrix_mod(df_passenger_all, df_tgt_st_code001, df_tgt_st_code002,
                                       by = c("st_name", "next_st_name"),
                                       depart = st_name, arrive = next_st_name, volume) %>%
           .[rownames(kanto_jr_adjacent_matrix), ] %>%
           .[, rownames(kanto_jr_adjacent_matrix)]
       },
    out_kanto_allpassenger_matrix_rds =
      kanto_jr_allpassernger_matrix %>%
      readr::write_rds("data/kanto_jr_allpassenger_matrix.rds")
  )

plan_station_numbering <-
  drake::bind_plans(
    plan_tokyo_mesh,
    plan_mlit_census,
    plan_st_geometry,
    plan_st_graph,
    plan_adj_matrix)

drake::make(plan_station_numbering)
# drake::loadd(list = c("sf_kanto",
#                       "sf_tokyo"))
# drake::loadd(list = c("df_passenger", "df_station_code"))
# drake::loadd(list = c("tokyo_railstation",
#                       "sf_target_st"))
# drake::loadd(list = c("df_tgt_st_code001",
#                       "df_tgt_st_code002",
#                       "df_st_graph",
#                       "routes_tidy"))
# drake::loadd(list = c("kanto_jr_adjacent_matrix",
#                       "kanto_jr_allpassernger_matrix",
#                       "df_passenger_all"))
