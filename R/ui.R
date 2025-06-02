#' Create Text Blocks for Annotation
#'
#' @param x character vector, texts to be annotated
#' @param tags character vector, identifier for texts, needs to be same length as x
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#' @param blockids character vector, ids of text blocks, new blocks will be created if left empty
#'
#' @return A character vector of block ids. It is necessary for later steps.
#' @export
#'
#' @examples
#' texts <- c("In a hole in the ground there lived a hobbit.",
#'             "It was a hobbit-hole, and that means comfort.")
#' \dontrun{
#' add_texts(texts,
#'           tags,
#'           "fra1",
#'           "SV_S3A96bzOnfyKMEDCiKhw",
#'           "ZAhIjt6CkPO5FyczlRhJ")
#' }
add_texts <- function(x, tags, DATA_CENTER, SURVEY_ID, API_TOKEN, blockids = NULL){
  if(is.null(blockids)){
    blockids <- create_blocks(ndoc = length(x),
                              tags = tags,
                              DATA_CENTER = DATA_CENTER,
                              SURVEY_ID = SURVEY_ID,
                              API_TOKEN = API_TOKEN)
  }
  for(i in seq_along(blockids)){
    add_text(block_id = blockids[i],
             text = x[i],
             tag = tags[i],
             DATA_CENTER = DATA_CENTER,
             SURVEY_ID = SURVEY_ID,
             API_TOKEN = API_TOKEN)
  }
  blockids
}


#' Add Questions to Text Blocks
#'
#' @param blockids character vector, ids of text blocks
#' @param question string, question for annotators
#' @param answers character vector, answers for annotators
#' @param tags character vector, identifier for texts, needs to be same length as x
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#' @param selector string, the format used for collecting responses, one of: "DL", "GRB", "MACOL", "MAHR", "MAVR", "MSB", "NPS", "SACOL", "SAHR", "SAVR", "SB", "TB", "TXOT", "PTB"
#' @param subselector string, additional options or variations for response collection, one of: "GR", "TX", "TXOT", "WOTXB", "WTXB"
#' @param forced string, force respondents to answer a question, one of: "ON", "OFF"
#'
#'
#' @return NULL
#' @export
#'
#' @examples
#' blockids <- c("BL_v7iQbRtKtAv7mQ10zO1h", "BL_t4VtQCrLDxzJalrgShVx")
#' question <- c("What does a Hobbit hole look like?")
#' answers <- c("nasty","dirty","comfortable")
#' \dontrun{
#' add_questions(blockids,
#'               question,
#'               answers,
#'               tags,
#'               "fra1",
#'               "SV_S3A96bzOnfyKMEDCiKhw",
#'               "ZAhIjt6CkPO5FyczlRhJ")
#' }
add_questions <- function(blockids, question, answers, tags, DATA_CENTER, SURVEY_ID, API_TOKEN,
                          selector = "SAVR", subselector = "TX", forced = "ON"){
  for(i in seq_along(blockids)){
    add_question(block_id = blockids[i],
                 question = question,
                 answers = answers,
                 tag = tags[i],
                 DATA_CENTER = DATA_CENTER,
                 SURVEY_ID = SURVEY_ID,
                 API_TOKEN = API_TOKEN,
                 qtype = "MC",
                 selector = selector,
                 subselector = subselector,
                 forced = forced)
  }
}

#' Create Randomizer Block
#'
#' @param blockids character vector, ids of text blocks
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#' @param present integer, how many blocks to show to each annotator
#' @param even boolean, should the randomizer distribute text evenly
#'
#' @return NULL
#' @export
#'
#' @examples
#' blockids <- c("BL_v7iQbRtKtAv7mQ10zO1h", "BL_t4VtQCrLDxzJalrgShVx")
#' \dontrun{
#' create_block_randomizer(blockids,
#'                         "fra1",
#'                         "SV_S3A96bzOnfyKMEDCiKhw",
#'                         "ZAhIjt6CkPO5FyczlRhJ")
#' }
create_block_randomizer <- function(blockids,
                                    DATA_CENTER,
                                    SURVEY_ID,
                                    API_TOKEN,
                                    present = 5,
                                    even = TRUE){
  from_block <- blockids[1]
  to_block <- blockids[length(blockids)]
  # Get flow
  payload <- get_flow(DATA_CENTER = DATA_CENTER,
                      SURVEY_ID = SURVEY_ID,
                      API_TOKEN = API_TOKEN)
  flow <- payload$Flow

  fromid <- listoflist_find(flow, "ID", from_block)
  toid <- listoflist_find(flow, "ID", to_block)

  # Replace flow
  block_randomizer <- list(
    Type = "BlockRandomizer",
    FlowID = "FL_999",
    SubSet = present,
    EvenPresentation = TRUE,
    Flow = flow[fromid:toid]
  )
  if(fromid == 1){
    pre <- NULL
  } else {
    pre <- flow[1:(fromid-1)]
  }
  if(toid == length(flow)){
    post <- NULL
  } else {
    post <- flow[(toid+1):length(flow)]
  }
  new_flow <- c(pre,
                list(block_randomizer),
                post)
  payload$Flow <- new_flow

  response <- httr::PUT(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID,"/flow"),
    httr::add_headers(.headers =  gen_header(API_TOKEN)),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE),
    encode = "json"
  )
  check_status(response)
}

#' Title
#'
#' @param DATA_CENTER string, your Qualtrics data center
#' @param SURVEY_ID string, your Qualtrics survey ID
#' @param API_TOKEN string, your Qualtrics API token
#'
#' @return json, survey setup
#' @export
#'
#' @examples
#' \dontrun{
#' get_survey(             "fra1",
#'                         "SV_S3A96bzOnfyKMEDCiKhw",
#'                         "ZAhIjt6CkPO5FyczlRhJ")
#' }
get_survey <- function(DATA_CENTER,SURVEY_ID,API_TOKEN){
  response <- httr::GET(
    paste0("https://",DATA_CENTER,".qualtrics.com/API/v3/survey-definitions/",SURVEY_ID),
    httr::add_headers(
      "X-API-TOKEN" = API_TOKEN,
      "Content-Type" = "application/json"
    )
  )
  res <-get_content(response)
  result <- res$result
  result
}
