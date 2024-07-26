#' historic_sequence
#'
#' Allow to perform a pivot wider on a sequencial dataset (here the type is dataframe), each variable will be dupplicated in a column to show the value to this variable at n - 1 for each individual, see examples.
#'
#' @param inpt_datf is the input dataframe
#' @param bf_ is the number of previous value of the individual it will search for, see examples
#'
#' @examples
#'
#' set.seed(123)
#' var1 <- round(runif(n = 14, min = 100, max = 122))
#' set.seed(123)
#' var2 <- round(runif(n = 14, min = 14, max = 20))
#' 
#' datf <- data.frame("ids" = c(20, 20, 20, 20, 19, 19, 19, 18, 18, 18, 18,
#'                             17, 17, 17),
#'                   "individual" = c("oui", "non", "peut1", "peut2",
#'                                    "oui", "peut1", "peut2"),
#'                   "var1" = var1,
#'                   "var2" = var2)
#' print(datf)
#'
#'    ids individual var1 var2
#' 1   20        oui  106   16
#' 2   20        non  117   19
#' 3   20      peut1  109   16
#' 4   20      peut2  119   19
#' 5   19        oui  121   20
#' 6   19      peut1  101   14
#' 7   19      peut2  112   17
#' 8   18        oui  120   19
#' 9   18        non  112   17
#' 10  18      peut1  110   17
#' 11  18      peut2  121   20
#' 12  17        oui  110   17
#' 13  17      peut1  115   18
#' 14  17      peut2  113   17
#'
#' historic_sequence(inpt_datf = datf, bf_ = 2)
#'
#'   id_seq individual var1-1 var1-2 var2-1 var2-2
#' 1     20        oui    121    120     20     19
#' 2     20        non     NA    112     NA     17
#' 3     20      peut1    101    110     14     17
#' 4     20      peut2    112    121     17     20
#' 5     19        oui    120    110     19     17
#' 6     19      peut1    110    115     17     18
#' 7     19      peut2    121    113     20     17
#'
#' historic_sequence(inpt_datf = datf, bf_ = 3)
#' 
#'   id_seq individual var1-1 var1-2 var1-3 var2-1 var2-2 var2-3
#' 1     20        oui    121    120    110     20     19     17
#' 2     20        non     NA    112     NA     NA     17     NA
#' 3     20      peut1    101    110    115     14     17     18
#' 4     20      peut2    112    121    113     17     20     17
#'
#' @export

historic_sequence <- function(inpt_datf, bf_ = 1){
  nb_vars <- ncol(inpt_datf) - 2
  cur_ids <- sort(x = unique(inpt_datf[, 1]), decreasing = TRUE)
  rtn_datf <- as.data.frame(matrix(data = NA, 
                nrow = sum(inpt_datf[, 1] %in% cur_ids[1:(length(cur_ids) - bf_)]), ncol = 2))
  colnames(rtn_datf)[c(1, 2)] <- c("id_seq", "individual")
  cur_datf <- as.data.frame(matrix(nrow = 0, ncol = bf_))
  I = 1
  individuals_v <- c()
  ids_v <- c()
  for (i in 1:(length(cur_ids) - bf_)){
    indivs <- inpt_datf[inpt_datf[, 1] == cur_ids[i], 2] 
    individuals_v <- c(individuals_v, indivs)
    ids_v <- c(ids_v, rep(x = cur_ids[i], times = length(indivs)))
    cur_datf2 <- as.data.frame(matrix(nrow = length(indivs), ncol = 0))
    for (i2 in 1:bf_){
      cur_inpt_datf <- inpt_datf[inpt_datf[, 1] == cur_ids[(i + i2)], (I + 2)]
      cur_indivs <- match(x = indivs, table = inpt_datf[inpt_datf[, 1] == cur_ids[(i + i2)], 2])   
      cur_inpt_datf <- cur_inpt_datf[cur_indivs[!(is.na(cur_indivs))]]
      for (e in grep(pattern = TRUE, x = is.na(cur_indivs))){
        cur_inpt_datf <- append(x = cur_inpt_datf, values = NA, after = (e - 1))
      }
      cur_datf2 <- cbind(cur_datf2, cur_inpt_datf)
    }
    cur_datf <- rbind(cur_datf, cur_datf2)
  }
  rtn_datf[, 2] <- individuals_v
  rtn_datf[, 1] <- ids_v
  rtn_datf <- cbind(rtn_datf, cur_datf)
  colnames(rtn_datf)[3:(2 + bf_)] <- paste(colnames(inpt_datf)[3], c(1:bf_), sep = "-") 
  if (nb_vars > 1){
    for (I in 2:nb_vars){
      cur_datf <- as.data.frame(matrix(nrow = 0, ncol = bf_))
      for (i in 1:(length(cur_ids) - bf_)){
        indivs <- inpt_datf[inpt_datf[, 1] == cur_ids[i], 2] 
        cur_datf2 <- as.data.frame(matrix(nrow = length(indivs), ncol = 0))
        for (i2 in 1:bf_){
          cur_inpt_datf <- inpt_datf[(inpt_datf[, 1] == cur_ids[(i + i2)]), (I + 2)]
          cur_indivs <- match(x = indivs, table = inpt_datf[inpt_datf[, 1] == cur_ids[(i + i2)], 2])   
          cur_inpt_datf <- cur_inpt_datf[cur_indivs[!(is.na(cur_indivs))]]
          for (e in grep(pattern = TRUE, x = is.na(cur_indivs))){
            cur_inpt_datf <- append(x = cur_inpt_datf, values = NA, after = (e - 1))
          }
          cur_datf2 <- cbind(cur_datf2, cur_inpt_datf)
        }
        cur_datf <- rbind(cur_datf, cur_datf2)
      }
      rtn_datf <- cbind(rtn_datf, cur_datf)
      colnames(rtn_datf)[((I - 1) * bf_ + 3):(I * bf_ + 2)] <- paste(colnames(inpt_datf)[(I + 2)], c(1:bf_), sep = "-")
    }
  }
  return(rtn_datf)
}


