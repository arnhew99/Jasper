xoxo <- function() {

	# some useful functions...

	ex <- function(x,y) {

		lines(x=x+c(-0.4,0.4), y=y+c(-0.4,0.4))
		lines(x=x+c(-0.4,0.4), y=y+c(0.4,-0.4))

	}

	oh <- function(x,y) {

		points(x=x, y=y, cex=15)

	}

	blankWindow <- function() {

		par(mar=c(6,6,6,6))
		par(lend=2)
		par(lwd=2)
		plot(x=0, y=0, xlim=c(0,3), ylim=c(3,0), axes=FALSE, pch=NA, xlab="", ylab="")

	}

	startingToken <- function() {

		blankWindow()

		lines(x=c(1.5,1.5), y=c(0,3))

		ex(0.5,1.5)
		oh(2.5,1.5)

		clickedPoint <- locator(n=1)
		return(ifelse(clickedPoint$x < 1.5,-1,1))

	}

	playerOrComputer <- function() {
		
		blankWindow()

		lines(x=c(1.5,1.5), y=c(0,3))

		points(x=0.5, y=1.5, cex=15, pch=0)
		points(x=0.5, y=1.5, cex=12, pch=0)

		oh(2.3,1.5)
		oh(2.7,1.5)

		clickedPoint <- locator(n=1)
		return(clickedPoint$x < 1.5)

	}

	drawGrid <- function() {

		blankWindow()

		lines(x=c(1,1), y=c(0,3))
		lines(x=c(2,2), y=c(0,3))
		lines(x=c(0,3), y=c(1,1))
		lines(x=c(0,3), y=c(2,2))

	}


	updateGrid <- function(currentTurn, currentGrid) {

		clickedPoint <- locator(n=1)

		clickedPoint <- c(min(c(max(c(0,floor(clickedPoint$x))),2)), min(c(max(c(0,floor(clickedPoint$y))),2)))

		if (currentGrid[clickedPoint[2]+1, clickedPoint[1]+1] != 0) {
			return(-10)
		}

		currentGrid[clickedPoint[2]+1, clickedPoint[1]+1] <- currentTurn

		if (currentTurn == -1) drawPt <- ex else drawPt <- oh
		drawPt(clickedPoint[1]+0.5, clickedPoint[2]+0.5)

		return(currentGrid)


	}

	checkGrid <- function(currentGrid) {

		cols <- colSums(currentGrid)
		rows <- rowSums(currentGrid)
		diag1 <- sum(diag(currentGrid))
		diag2 <- sum(c(currentGrid[3,1], currentGrid[2,2], currentGrid[1,3]))

		# print(cols)
		# print(rows)
		# print(diag1)
		# print(diag2)

		return(any(any(cols==3), any(cols==-3), any(rows==3), any(rows==-3), diag1==3, diag1==-3, diag2==3, diag2==-3))

	}


	solveRandom <- function(currentTurn, currentGrid) {

		# print(currentTurn)
		# print(currentGrid) 

		result <- checkGrid(currentGrid)
		if (result) {
			# check to see if anyone has won
			# (in which case it wasn't the current player)
			return((-1)*currentTurn)
		} else if (!any(currentGrid == 0)) {
			# check to see if it's a draw
			return(0)
		} else {
			# otherwise pick randomly and run the next turn
			availableLocs <- which(currentGrid == 0, arr.ind=TRUE)

			## pick randomly from the available spots		
			chosenPoint <- availableLocs[sample.int(dim(availableLocs)[1], size=1),]

			# cat("computer chooses\n")
			# print(chosenPoint)

			currentGrid[chosenPoint[1], chosenPoint[2]] <- currentTurn

			solveRandom((-1)*currentTurn, currentGrid)
		}


	}

	moveComputer2 <- function(currentTurn, currentGrid, maxIterations) {

		# check what positions are available 
		availableLocs <- which(currentGrid == 0, arr.ind=TRUE)
		numberAvailableLocs <- dim(availableLocs)[1]

		# test to see if the other player can win easily 
		testResults <- rep(FALSE, numberAvailableLocs)
		for (ii in 1:numberAvailableLocs) {

			testGrid <- currentGrid
			testGrid[availableLocs[ii,1], availableLocs[ii,2]] <- (-1)*currentTurn

			testResults[ii] <- checkGrid(testGrid)

		}

		if (any(testResults)) {

			# opposite player will win!
			# so oppose
			cat("Computer turn forced...!\n")
			if (sum(testResults) > 1) cat("Computer will lose... :(\n")

			chosenPoint <- availableLocs[which(testResults)[1],]

		} else {

			currentTurnWins <- rep(0, numberAvailableLocs)
			oppositeWins <- rep(0, numberAvailableLocs)

			for (ii in 1:numberAvailableLocs) {

				testGrid <- currentGrid
				testGrid[availableLocs[ii,1], availableLocs[ii,2]] <- currentTurn

				# now test outcomes...
				outcomesCount <- sapply(rep((-1)*currentTurn,maxIterations), solveRandom, testGrid)

				outcomes <- sapply(c(-1,0,1), function(jj) sum(outcomesCount==jj))

				currentTurnWins[ii] <- outcomes[ifelse(currentTurn==-1,1,3)]
				oppositeWins[ii] <- outcomes[ifelse(currentTurn==-1,3,1)]

			}

			# now pick a move

			# maximise win for current player...?
			# chosenPoint <- availableLocs[which.max(currentTurnWins),]

			# minimise win for opposite player
			chosenPoint <- availableLocs[which.min(oppositeWins),]


		}

		cat(paste0("Computer plays ", paste(chosenPoint, collapse="-"), "\n"))
		flush.console()


		currentGrid[chosenPoint[1], chosenPoint[2]] <- currentTurn

		if (currentTurn == -1) drawPt <- ex else drawPt <- oh
		drawPt(chosenPoint[2]-0.5, chosenPoint[1]-0.5)

		return(currentGrid)

	}



	
	## code to draw starts here...

	computerPlays <- playerOrComputer()

	cat(paste0(ifelse(computerPlays, "Computer is playing", "Two players!"), "\n"))


	currentTurn <- startingToken()
	result <- FALSE
	playerTurn <- TRUE

	

	drawGrid()

	currentGrid <- matrix(0, ncol=3, nrow=3)

	

	while (!result) {

		if (playerTurn) {
			newGrid <- updateGrid(currentTurn, currentGrid)
			if (newGrid[1] == -10) {
				cat("This square is filled, try again\n")
				next
			} else {
				currentGrid <- newGrid
			}
		} else {
				currentGrid <- moveComputer2(currentTurn, currentGrid, maxIterations=1000)
				
		}

		if (computerPlays) {
			playerTurn <- !playerTurn
		}
		currentTurn <- ifelse(currentTurn==1,-1,1)
		# print(currentGrid)
		# flush.console()

		result <- checkGrid(currentGrid)

		if (result) cat(paste0(ifelse(currentTurn==1,"X","O"), " is the winner!\n"))
		else if (!any(currentGrid==0)) {
			cat("Draw!\n")
			break
		}

	}

}

