# Some Rcmdr dialogs for the doBy package
# Last modified: 12 December 2011 by Jonathan Lee

# Note: the following function (with contributions from Richard Heiberger) 
# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded

.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
    }
}

summaryByGUI <- function(){
  require(doBy)
  .activeDataSet <- ActiveDataSet()
	initializeDialog(title=gettextRcmdr("Summary by..."))
	dsname <- tclVar("")
	dsnameFrame <- tkframe(top)
	entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
	variablesBox <- variableListBox(top, Variables(), title=gettextRcmdr("Variables to compute statistics on\n(leave blank for all numerical)"), selectmode="multiple")
	byBox <- variableListBox(top, Variables(), title=gettextRcmdr("Group by\n(leave blank for no grouping)"), selectmode="multiple")
	
  # Choose statistic
  radioButtons(name="statistic", buttons=c("mean", "median", "mode", "var"), labels=gettextRcmdr(c("Mean", "Median", "Mode", "Variance")), title=gettextRcmdr("Statistic"))
	otherVariable <- tclVar("")
	otherButton <- ttkradiobutton(statisticFrame, variable=statisticVariable, value="other")
	otherEntry <- ttkentry(statisticFrame, width="20", textvariable=otherVariable)   
	tkgrid(labelRcmdr(statisticFrame, text=gettextRcmdr("Other (specify)")), otherButton, otherEntry, sticky="w")
	onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		
    # If variable name not blank, make sure name is valid
    if (dsnameValue != "") {
			if (!is.valid.name(dsnameValue)) {
  		  errorCondition(recall=summaryByGUI,
			  	message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
		  	return()
		  }
		}
        
    #Check if need to overwrite existing variable on save
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
				summaryByGUI()
				return()
			}
		}
    
		variables <- getSelection(variablesBox)
		byVariables <- getSelection(byBox)
		
    # Check if no variables selected, then use all variables
    if (length(variables) == 0){
			variables = "."
		}
    
    # Check if no variables to group by, then use no grouping
		if (length(byVariables) == 0){
			byVariables = "1"
		}
    
    # Get statistic to be calculated
		statistic <- tclvalue(statisticVariable)
		if (statistic == "other") statistic <- tclvalue(otherVariable)
    
		vars <- paste(variables, collapse="+")
    by <- paste(byVariables, collapse="+")
      	
    command <- paste("summaryBy(", vars, "~", by, ", data=", .activeDataSet, ", FUN=c(", statistic, "))", sep="")
    
    # if to save to variable, then add that in
    if (dsnameValue != "") {
      command <- paste(dsnameValue, " <- ", command, sep="")
    }
    
		doItAndPrint(command)
    if (dsnameValue != "") activeDataSet(dsnameValue)
		
		closeDialog()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(help="summaryby")
	tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Name for resulting data frame (leave blank to just print):  ")), entryDsname)
	tkgrid(dsnameFrame, sticky="w", columnspan=2)
	tkgrid(getFrame(variablesBox), getFrame(byBox), sticky="nw")
	tkgrid(statisticFrame, sticky="w", columnspan=2)
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(rows=5, columns=2)
}

orderByGUI <- function(){
  require(doBy)
  .activeDataSet <- ActiveDataSet()
  initializeDialog(title=gettextRcmdr("Order by..."))
	dsname <- tclVar("OrderedData")
	dsnameFrame <- tkframe(top)
	entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
	order1Box <- variableListBox(top, Variables(), title=gettextRcmdr("First variable to sort by\n"), selectmode="single")
	order2Box <- variableListBox(top, Variables(), title=gettextRcmdr("Second variable to sort by\n(leave blank for none)"), selectmode="single")
  order3Box <- variableListBox(top, Variables(), title=gettextRcmdr("Third variable to sort by\n(leave blank for none)"), selectmode="single")
  
  checkBoxes(frame="checkBoxFrame", boxes=c("dir1", "dir2", "dir3"), initialValues=c("0", "0", "0"), labels=gettextRcmdr(c("Sort first variable decreasing", "Sort second variable decreasing", "Sort third variable decreasing")))

  
	onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		
    # If variable name not blank, make sure name is valid
    if (dsnameValue != "") {
			if (!is.valid.name(dsnameValue)) {
  		  errorCondition(recall=orderByGUI,	message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
		  	return()
		  }
		}
        
    #Check if need to overwrite existing variable on save
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
				orderByGUI()
				return()
			}
		}
    
		order1 <- getSelection(order1Box)
		order2 <- getSelection(order2Box)
    order3 <- getSelection(order3Box)
		
    # Check if no variables selected, then throw error
    if (length(order1) == 0){
			errorCondition(recall=orderByGUI,	message=gettextRcmdr("Must select at least one variable to order by."))
		  return()
		} else {
      if (tclvalue(dir1Variable) == 1) {
        vars <- paste("-",order1,sep="")
      } else {
        vars <- order1
      }
		}
    
    if (length(order2) > 0){
      if (tclvalue(dir2Variable) == 1) {
        vars <- paste(vars,"-",order2,sep="")
      } else {
        vars <- paste(vars,"+",order2,sep="")
      }
		}
    
    if (length(order3) > 0){
  		if (tclvalue(dir3Variable) == 1) {
        vars <- paste(vars,"-",order3,sep="")
      } else {
        vars <- paste(vars,"+",order3,sep="")
      }
		}
    
    command <- paste("orderBy(~", vars, ", data=", .activeDataSet, ")", sep="")
    
    # if to save to variable, then add that in
    if (dsnameValue != "") {
      command <- paste(dsnameValue, " <- ", command, sep="")
    }
    
		doItAndPrint(command)
    if (dsnameValue != "") activeDataSet(dsnameValue)
		
		closeDialog()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(help="orderby")
	tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Name for resulting data frame (leave blank to just print):  ")), entryDsname)
	tkgrid(dsnameFrame, sticky="w", columnspan=3)
	tkgrid(getFrame(order1Box), getFrame(order2Box), getFrame(order3Box), sticky="nw")
  tkgrid(checkBoxFrame,sticky="w",columnspan=3)
	tkgrid(buttonsFrame, sticky="w", columnspan=3)
	dialogSuffix(rows=10, columns=3)
}

splitByGUI <- function(){
  require(doBy)
  .activeDataSet <- ActiveDataSet()
  initializeDialog(title=gettextRcmdr("Split by..."))
	dsname <- tclVar("SplitData")
	dsnameFrame <- tkframe(top)
	entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
	byBox <- variableListBox(top, Variables(), title=gettextRcmdr("Variable(s) to split by\n(select one or more)"), selectmode="multiple")
	
  onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		
    # If variable name not blank, make sure name is valid
    if (dsnameValue != "") {
			if (!is.valid.name(dsnameValue)) {
  		  errorCondition(recall=splitByGUI, message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
		  	return()
		  }
		}
        
    #Check if need to overwrite existing variable on save
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
				splitByGUI()
				return()
			}
		}
    
		byVariables <- getSelection(byBox)
		
    # Check if no variables selected, then use all variables
    if (length(byVariables) == 0){
			errorCondition(recall=splitByGUI,	message=gettextRcmdr("Must select at least one variable to split by"))
      return()
		}
    
    by <- paste(byVariables, collapse="+")
      	
    command <- paste("splitBy(~", by, ", data=", .activeDataSet, ")", sep="")
    
    # if to save to variable, then add that in
    if (dsnameValue != "") {
      command <- paste(dsnameValue, " <- ", command, sep="")
    }
    
		doItAndPrint(command)
    #if (dsnameValue != "") activeDataSet(dsnameValue)
		
		closeDialog()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(help="splitby")
	tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Name for resulting data frame (leave blank to just print):  ")), entryDsname)
	tkgrid(dsnameFrame, sticky="w", columnspan=1)
	tkgrid(getFrame(byBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w", columnspan=1)
	dialogSuffix(rows=4, columns=1)
}

sampleByGUI <- function(){
  require(doBy)
  .activeDataSet <- ActiveDataSet()
  initializeDialog(title=gettextRcmdr("Split by..."))
  dsname <- tclVar("SampledData")
	dsnameFrame <- tkframe(top)
	entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
	byBox <- variableListBox(top, Variables(), title=gettextRcmdr("Variable(s) to sample by\n(select zero or more)"), selectmode="multiple")
	
  fracFrame <- tkframe(top)
  fracVariable <- tclVar("0.1")
	fracEntry <- ttkentry(fracFrame, width="5", textvariable=fracVariable)   
	tkgrid(labelRcmdr(fracFrame, text=gettextRcmdr("Fraction to sample")), fracEntry, sticky="w")
  checkBoxes(frame="checkBoxFrame", boxes=c("replace", "systematic"), initialValues=c("0", "0"), labels=gettextRcmdr(c("Sample with Replacement", "Systematic Sampling")))
  
  onOK <- function(){
		dsnameValue <- trim.blanks(tclvalue(dsname))
		
      # If variable name not blank, make sure name is valid
    if (dsnameValue != "") {
			if (!is.valid.name(dsnameValue)) {
  		  errorCondition(recall=sampleByGUI, message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
		  	return()
		  }
		}
        
    #Check if need to overwrite existing variable on save
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
				sampleByGUI()
				return()
			}
		}
    
		byVariables <- getSelection(byBox)
		
    by <- paste(byVariables, collapse="+")
    
    frac <- tclvalue(fracVariable)

    if (tclvalue(replaceVariable) == 1) {
      replace <- "replace=TRUE, "
    } else {
      replace <- ""
    }
      
    if (tclvalue(systematicVariable) == 1) {
      systematic <- "systematic=TRUE, "
    } else {
      systematic <- ""
    }

    command <- paste("sampleBy(~", by, ", frac=", frac, ", ", replace, systematic, "data=", .activeDataSet, ")", sep="")
    
    # if to save to variable, then add that in
    if (dsnameValue != "") {
      command <- paste(dsnameValue, " <- ", command, sep="")
    }
    
		doItAndPrint(command)
    if (dsnameValue != "") activeDataSet(dsnameValue)
		
		closeDialog()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(help="sampleby")
	tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Name for resulting data frame (leave blank to just print):  ")), entryDsname)
	tkgrid(dsnameFrame, sticky="w", columnspan=2)
	tkgrid(getFrame(byBox), sticky="w")
  tkgrid(fracFrame, checkBoxFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(rows=6, columns=2)
}

