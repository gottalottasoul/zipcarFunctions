#' Detach All Packages
#'
#' This function will unload all previously loaded packages, with the exception(s) of base packages
#' detachAllPackages()

detachAllPackages <- function() {
  #from  http://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
  basic.packages.blank <-  c("stats"
                             ,"graphics"
                             ,"grDevices"
                             ,"utils"
                             ,"datasets"
                             ,"methods"
                             ,"base"
                             ,"zipcarFunctions")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")

  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                  TRUE,
                                  FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

#' Load Member Data files
#'
#' This function leverages the readr package to read and properly format Member specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/Members/
#' @param my.file the name of the file to load.  Required.
#' getMemberData()

getMemberData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/Members/"
  }

  my.file<-paste0(my.path,my.file)
  members.all=read_delim(my.file
                         ,"\t"
                         ,col_types = cols(col_character() #Member ID
                                           ,col_character() #Zipfleet
                                           ,col_character() #Business Segment
                                           ,col_character() #Rate Plan
                                           ,col_date("%m/%d/%Y") #First Join
                                           ,col_date("%m/%d/%Y") #First Res
                                           ##make sure you are using the updated member export with number of joins
                                           #                                         ,col_integer()
                                           #                                         ,col_character()))
                                           ,col_character() #Latitude
                                           ,col_character() #Longitude
                                           ,col_character() #Address Type
                                           ,col_date("%m/%d/%Y")))
  members.all
}

#' Load Reservation Data files
#'
#' This function leverages the readr package to read and properly format Reservation specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/Reservations/
#' @param my.file the name of the file to load.  Required.
#' getReservationData()

getReservationData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/Reservations/"
  }

  my.file<-paste0(my.path,my.file)
  reservations.all=read_delim(my.file
                              ,"\t"
                              ,col_types = cols(col_character() #Member ID
                                                ,col_character() #Mem Zipfleet
                                                ,col_character() #Loc Zipfleet
                                                ,col_character() #Segment
                                                ,col_datetime("%m/%d/%Y %H:%M") #Res Date
                                                ,col_character() #Res ID
                                                ,col_number() #Revenue
                                                ,col_number() #Penalty
                                                ,col_number() #Distance
                                                ,col_number() #Hours
                                                ,col_number() #Drive Time
                                                ,col_number() #Res Lead time
                                                ,col_integer() #Web Res indicator
                                                ,col_integer() #App Res indicator
                                                ,col_integer() #Hourly Res indicator
                                                ,col_integer() #Daily Res indicator
                                                ,col_integer() #Overnight Res indicator
                                                ,col_integer() #Oneway Res indicator
                                                ,col_character() #Source
                                                ,col_character() #Rated as
                                                ,col_character() #Res Type
                                                ,col_character() #Vehicle Class
                                                ,col_character() #Location type
                                                ,col_character() #Location ID
                                                ,col_character() #Location
                                                ,col_character()#Zone
                                                ,col_number() #Res Lat
                                                ,col_number() #Res Long
                                                ,col_number() #Member Lat
                                                ,col_number() #Member Long
                                                ,col_integer() #Overall Res Seq
                                                ,col_integer()) #Cancelled Res
  )
  # reservations.all<-data.table(reservations.all)
  #reservations.cancels<-subset(reservations.all,CANCELLED_RES==1)
  reservations.all<-subset(reservations.all,CANCELLED_RES==0)
  reservations.all<-subset(reservations.all,CANCELLED_RES==0)
}


#' Load Customer Lifetime Value Data file
#'
#' This function leverages the readr package to read and properly format Member CLV specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/Reservations/
#' @param my.file the name of the file to load.  Required.
#' getCLVReservationData()

getCLVReservationData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/Reservations/"
  }

  my.file<-paste0(my.path,my.file)
  reservations.all=read_delim(my.file
                              ,"\t"
                              ,col_types = cols(col_character()
                                                ,col_date("%m/%d/%Y")
                                                ,col_date("%m/%d/%Y")
                                                ,col_number()))
  reservations.all
}

#' Load Member Survival Data files
#'
#' This function leverages the readr package to read and properly format Member Survival specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/Survival/
#' @param my.file the name of the file to load.  Required.
#' getMemberSurvivalData()

getMemberSurvivalData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/Survival/"
  }

  my.file<-paste0(my.path,my.file)
  member.survival=read_delim(my.file
                             ,"\t"
                             ,col_types = cols(col_character() #member_id
                                               ,col_date("%m/%d/%Y") #join date
                                               ,col_date("%m/%d/%Y") # leave date
                                               ,col_integer() #event flag
                             ))
  #                                                ,col_character()
  #                                                ,col_character()
  #                                                ,col_character()
  #                                                ,col_character()
  #                                                ,col_character()
  #                                                ,col_number()
  #                                                ,col_integer()))

  member.survival
}

#' Load Member Location Data files
#'
#' This function leverages the readr package to read and properly format Member Location specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/Geo/
#' @param my.file the name of the file to load.  Required.
#' getMemberLocationData()

getMemberLocationData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/Geo/"
  }

  my.file<-paste0(my.path,my.file)
  members.location=read_delim(my.file
                              ,"\t"
                              ,col_types = cols(col_character() #member_id
                                                ,col_character() #zipfleet
                                                ,col_character() #bus segment
                                                ,col_character() #rate plan
                                                ,col_date("%m/%d/%Y") #join date
                                                ,col_character() #join cohort
                                                ,col_character() #preferred flag
                                                ,col_character() #addr type
                                                ,col_number() #long
                                                ,col_number() #lat
                                                ,col_number() #res
                                                ,col_number() #rev
                                                ,col_character() #snapshot cohort
                                                ,col_date("%m/%d/%Y") #snapshot date
                              ))

  #  members.location<-data.table(members.location)
  members.location
}

#' Load Pod Data files
#'
#' This function leverages the readr package to read and properly format Pod Locations specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/Pod Locations/
#' @param my.file the name of the file to load.  Required.
#' getPodLocationData()

getPodLocationData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/Pod Locations/"
  }

  my.file<-paste0(my.path,my.file)
  pod.location=read_delim(my.file
                          ,"\t"
                          ,col_types = cols(col_character() #location_id
                                            ,col_character() #metro
                                            ,col_character() #zone
                                            ,col_character() #location
                                            ,col_character() #zip
                                            ,col_number() #lat
                                            ,col_number() #long
                                            ,col_character() #zipfleet
                          ))

  #pod.location<-data.table(pod.location)
  pod.location
}

#' Load Member Closest Pod Data files
#'
#' This function leverages the readr package to read and properly format Member Closest Pod Location specific data files
#' @param my.path the directory path to load the file.  Default path is ../../Data/geo/
#' @param my.file the name of the file to load.  Required.
#' getMemberClosestPodLocationData()

getMemberClosestPodLocationData <- function(my.file,my.path)
{
  if (missing(my.path))
  {
    my.path="../../Data/geo/"
  }

  my.file<-paste0(my.path,my.file)
  pod.location=read_delim(my.file
                          ,"\t"
                          ,col_types = cols(col_character() #member_id
                                            ,col_character() #zipfleet
                                            ,col_character() #bus segment
                                            ,col_character() #rate plan
                                            ,col_date("%m/%d/%Y") #join date
                                            ,col_character() #join cohort
                                            ,col_character() #address id
                                            ,col_character() #preferred flag
                                            ,col_character() #addr type
                                            ,col_number() #long
                                            ,col_number() #lat
                                            ,col_number() #res
                                            ,col_number() #rev
                                            ,col_character() #snapshot cohort
                                            ,col_date("%m/%d/%Y") #snapshot date
                                            ,col_integer() #pod index
                                            ,col_number() #lat
                                            ,col_number() #long
                                            ,col_character() #location id
                                            ,col_character() #pod name
                                            ,col_number() #pod distance
                                            ,col_integer() #outlier flag
                          ))

  #pod.location<-data.table(pod.location)
  pod.location
}



#' multiplot
#'
#' A function for printing multiple ggplot plots on a grid.  Reproduced from cookbook-r.com
#' multiplot()



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' Crosstab
#'
#' This is a reproduction of the crosstab function created by Dr. Paul Williamson
#' crosstab()


crosstab <- function (..., dec.places = NULL,type = NULL,style = "wide",row.vars = NULL,col.vars = NULL,percentages = TRUE, addmargins = TRUE,subtotals=TRUE) {

  ###################################################################################
  #                                                                                 #
  # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
  # School of Environmental Sciences, University of Liverpool, UK.                  #
  #                                                                                 #
  # Adapted from the function ctab() in the catspec packge.                         #
  #                                                                                 #
  # Version: 12th July 2013                                                         #
  #                                                                                 #
  # Output best viewed using the companion function print.crosstab()                #
  #                                                                                 #
  ###################################################################################


  #Declare function used to convert frequency counts into relevant type of proportion or percentage

  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars),
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percentages) {
      tbl <- tbl * 100
    }
    tbl
  }

  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars


  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(dec.places) == dec.places, dec.places > -1)
  #type: see next section of code
  stopifnot(is.character(style))
  stopifnot(is.logical(percentages))
  stopifnot(is.logical(addmargins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)

  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types

  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables

  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count
    type <- "frequency"
  }



  #Check for integrity of requested analysis and adjust values of function arguments as required

  if ((addmargins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (addmargins=FALSE)")
    subtotals <- TRUE
  }

  if ((n.vars>1) & (length(type)>1) & (addmargins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }

  if ((length(type)>1) & (subtotals==FALSE)) {
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }

  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }

  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE))
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")


  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }

  #If dec.places not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(dec.places)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      dec.places <- 0
    } else {
      dec.places <-2
    }
  }

  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'

  args <- list(...)

  if (length(args) > 1) {
    if (!all(sapply(args, is.factor)))
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("dec.places", "type", "style", "percentages",
                    "addmargins", "subtotals")) if (is.null(get(opt)))
                      assign(opt, eval(parse(text = paste("tbl$", opt,
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }

  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))

  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }

  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (crosstab) which stores results as percentages if a percentage table type is requested.
  if (type[1] == "frequency")
    crosstab <- tbl
  else
    crosstab <- mk.pcnt.tbl(tbl, type[1])


  #If multiple table types requested, create and add these to
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency")
        crosstab <- tbl
      else crosstab <- mk.pcnt.tbl(tbl, type[i])
      crosstab <- as.data.frame.table(crosstab)
      crosstab[z] <- i
      tbldat <- rbind(tbldat, crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(crosstab))[z - 1] <- ""
  }


  #Add margins if required, adding only those margins appropriate to user request
  if (addmargins==TRUE) {

    vars <- c(row.vars,col.vars)

    if (length(type)==1) {
      if (type=="row.pct")
      { crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else
      { if (type=="column.pct")
      { crosstab <- addmargins(crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else
        { if (type=="joint.pct")
        { crosstab <- addmargins(crosstab,margin=c(vars[(n.row.vars)],vars[n.vars]))
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars]))
        }
          else #must be total.pct OR frequency
          { crosstab <- addmargins(crosstab)
          tbl <- addmargins(tbl)
          }
        }
      }
    }

    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }

  }


  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {

    #Create version of crosstab in ftable format
    t1 <- crosstab
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)

    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)

    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))

    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]

    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals)
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]
      }
    }

    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals

    t1 <- t1[((lab==0) | (lab==n.row.vars)),]

    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""

    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL

  }



  #Create output object 'result' [class: crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$dec.places <- dec.places
  result$type <- type
  result$style <- style
  result$percentages <- percentages
  result$addmargins <- addmargins
  result$subtotals <- subtotals

  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$crosstab <- crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]
  result$crosstab.nosub <- t1  #crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]
  class(result) <- "crosstab"

  #Return 'result' as output of function
  result

}

#' Print Crosstab
#'
#' This is a reproduction of the print.crosstab function created by Dr. Paul Williamson
#' print.crosstab()


print.crosstab <- function(x,dec.places=x$dec.places,subtotals=x$subtotals,...) {

  ###################################################################################
  #                                                                                 #
  # Function created by Dr Paul Williamson, Dept. of Geography and Planning,        #
  # School of Environmental Sciences, University of Liverpool, UK.                  #
  #                                                                                 #
  # Adapted from the function print.ctab() in the catspec packge.                   #
  #                                                                                 #
  # Version: 12th July 2013                                                         #
  #                                                                                 #
  # Designed to provide optimal viewing of the output from crosstab()               #
  #                                                                                 #
  ###################################################################################

  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars

  if (length(x$type)>1) {
    z<-length(names(dimnames(x$crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z)
    } else {
      col.vars<-c(z,col.vars)
    }
  }

  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$crosstab,x$dec.places))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$crosstab,x$dec.places))
    }
  }


  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {

    tbl <- ftable(x$crosstab,row.vars=row.vars,col.vars=col.vars)

    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,dec.places)
    print(tbl,...)

  }

  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {

    t1 <- x$crosstab.nosub

    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    dec.places <- x$dec.places
    number.format <- paste("%",width,".",dec.places,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))

    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])

    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }

    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)

  }

}

diagPlot<-function(model){
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()

  p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
  p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2<-p2+ggtitle("Normal Q-Q")+theme_bw()

  p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
  p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()

  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()

  p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggtitle("Residual vs Leverage Plot")
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5<-p5+theme_bw()+theme(legend.position="bottom")

  p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6<-p6+theme_bw()

  return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}


#' Five Thirty Eight Theme
#'
#' This function creates a 538 theme for ggplot graphics
#' fte_theme()


fte_theme <- function() {
  # from http://minimaxir.com/2015/02/ggplot-tutorial/

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]

  # Begin construction of chart
  theme_bw(base_size=9) +

    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid
    #    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.major.x = element_blank() ,
          # explicitly set the horizontal lines (or they will disappear too)
          panel.grid.major.y = element_line( size=.1, color="black" )) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25,hjust = 0.5)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
