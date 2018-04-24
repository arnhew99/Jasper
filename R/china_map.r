china_map <- function(type, 
						filestem, 
						table_csv=NULL, 
						table_cols=NULL, 
						table_headings=NULL, 
						sort_by=NULL, 
						sort_decreasing=FALSE,
						add_points_on_map=TRUE,
						add_points_by_table=TRUE,
						join_map_table_points=TRUE,
						join_lines_colour="grey50",
						region_name_heading="Region",
						split_urban_rural=FALSE,
						pchs=c(rep(15,5), rep(19,5)), 
						cols=rep(1,10), 
						cex_text=2.7, 
						pt_scaling=1.2,
						map_pt_scaling=pt_scaling,
						colspacing_factor=4,
						line_spacing=1,
						tableleft=0.92,
						tabletop=0.9) 
{

	
	# argument checking
	if (all(!is.null(table_headings), length(table_cols) != length(table_headings))) stop("Number of table headings doesn't match number of columns")
	
	if (length(pchs) != 10) stop("Not enough pchs")
	if (length(cols) != 10) stop("Not enough colours")


	# attempt to see if all the packages we need are installed
	for (pack in c("maps", "mapdata", "mapproj")) {
	
		# if (!(pack %in% installed.packages()[,1])) {
		if (length(find.package(pack, quiet=TRUE)) == 0) {
		
			cat(paste0("performing one-time installation of ", pack, " package\n"))
			flush.console()
			install.packages(pack, repos="https://cloud.r-project.org")
		}
	
	}
	
	require(mapdata)
	require(mapproj)
	
	SetPage(type=type, orient="LANDSCAPE", perpage=2, filestem=filestem)
	
	par(lwd=2)
	par(lend=2)
	par(xpd=NA)
	fit <- map("china", mar=c(7,12,7,12), projection="mercator")
	
	rect(xleft=-0.5, ybottom=0, xright=0.5, ytop=0.23, border=NA, col="white")

	reg_data <- structure(list(region_code = c(12L, 16L, 26L, 36L, 46L, 52L, 58L, 68L, 78L, 88L), 
								region_is_urban = c(1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L), 
								region_name = c("Qingdao", "Harbin", "Haikou", "Suzhou", "Liuzhou", "Sichuan", "Gansu", "Henan", "Zhejiang", "Hunan"), 
								city_name = c("Qingdao", "Harbin", "Haikou", "Suzhou", "Liuzhou", "Pengzhou", "Tianshui", "Huixian", "Tongxiang", "Liuyang"), 
								province_name = c("Shandong", "Heilongjiang", "Hainan", "Jiangsu", "Guangxi", "Sichuan", "Gansu", "Henan", "Zhejiang", "Hunan"), 
								north_rank = c(2L, 1L, 10L, 5L, 9L, 6L, 4L, 3L, 7L, 8L),
								latitude = c(36.08, 45.75, 20.03, 31.3, 24.32, 30.98, 34.58, 35.46, 30.63, 28.14), 
								east_rank = c(4L, 1L, 7L, 2L, 8L, 10L, 9L, 5L, 3L, 6L), 
								longitude = c(120.33, 126.63, 110.34, 120.6, 109.38, 103.93, 105.73, 113.8, 120.53, 113.63), 
								coastal_rank = c(1L, 6L, 2L, 4L, 5L, 9L, 10L, 8L, 3L, 7L), 
								pc1 = c(-0.00309, -0.00294, 0.005266, -3e-04, 0.005499, 0.001607, -0.00282, -0.00365, 0.000231, 0.002717), 
								pc2 = c(0.000376, -0.00017, 0.001064, -0.00071, 0.001911, -0.00016, 0.000174, 0.000158, -0.00093, -0.00077), 
								participant_count = c(35509L, 57555L, 29689L, 53260L, 50173L, 55687L, 50041L, 63357L, 57704L, 59916L)), 
								.Names = c("region_code", "region_is_urban", "region_name", "city_name", "province_name", "north_rank", "latitude", "east_rank", "longitude", "coastal_rank", "pc1", "pc2", "participant_count"), 
								class = "data.frame", row.names = c(NA, -10L))
								
	projected <- mapproject(reg_data$longitude, reg_data$latitude)
	
	table_dat <- read.csv(file=table_csv, as.is=TRUE, colClasses="character")
	
	# reorder the table to be in the same order as the plotting information
	# and then at writing time reorder the columns on the fly
	table_dat <- table_dat[order(as.numeric(table_dat$region_code)),]
	
	if (!is.null(sort_by)) {
		if (sort_decreasing) {
			neworder <- order(table_dat[,sort_by], decreasing=TRUE)
		} else {
			neworder <- order(table_dat[,sort_by])
		}
	} else if ("table_order" %in% names(table_dat)) {
		neworder <- table_dat$table_order
	} else {
		neworder <- 1:10
	}
	
	
	colspacing <- colspacing_factor*strwidth(" ", cex=cex_text)
	
	# calculate where rows in the table will appear
	table_rows_y <- tabletop - c(0,line_spacing*cumsum(rep(strheight("A\nA", cex=cex_text) - 1*strheight("A", cex=cex_text),9))) - 0.5*strheight("A", cex=cex_text)
	
	# calculate X positions of points
	if (split_urban_rural) {
	
		# add headings
		text(x=tableleft - colspacing - strwidth("Rural", cex=cex_text, font=2), y=tabletop - 0.5*diff(table_rows_y[1:2]), labels="Rural", cex=cex_text, adj=c(0,0), font=2)
		text(x=tableleft - 2*colspacing - strwidth("RuralUrban", cex=cex_text, font=2), y=tabletop - 0.5*diff(table_rows_y[1:2]), labels="Urban", cex=cex_text, adj=c(0,0), font=2)
		
		# calculate X positions
		pts_x <- tableleft - ifelse(reg_data$region_is_urban == 1L, 2*colspacing + 1*strwidth("Rural", cex=cex_text, font=2) + 0.5*strwidth("Rural", cex=cex_text, font=2), 1*colspacing + 0.5*strwidth("Rural", cex=cex_text, font=2))
		pts_x <- pts_x[neworder]
	} else {
		pts_x <- rep(tableleft - 1.5*colspacing,10)
	}
	# add points by the table, points on the map, and lines joining the two if requested
	if (all(add_points_by_table, add_points_on_map, join_map_table_points)) {
		for (i in 1:10) {
			lines(x=c(pts_x[i], (projected$x[neworder])[i]), y=c((table_rows_y)[i], (projected$y[neworder])[i]), lty="dashed", col=join_lines_colour, lend=2)
		}
	}
	if (add_points_by_table) {
		points(x=pts_x, y=table_rows_y, cex=pt_scaling*cex_text, pch=pchs[neworder], col=cols[neworder])
	}
	if (add_points_on_map) {
		points(x=projected$x, y=projected$y, pch=pchs, col=cols, cex=map_pt_scaling*cex_text)
	}
			
		
	
	
	# add the region name column
	text(x=tableleft, y=table_rows_y, labels=reg_data$region_name[neworder], adj=c(0,0.5), cex=cex_text)
	text(x=tableleft, y=tabletop - 0.5*diff(table_rows_y[1:2]), labels=region_name_heading, adj=c(0,0), font=2, cex=cex_text)
	
	
	# add the additional columns	
	if (!is.null(table_cols)) {
	
		newcol_pos <- tableleft + max(c(sapply(reg_data$region_name, strwidth, cex=cex_text), strwidth(region_name_heading, cex=cex_text, font=2))) + colspacing
		
		for (ii in 1:length(table_cols)) {
		
			text(x=newcol_pos, y=table_rows_y, labels=table_dat[neworder,table_cols[ii]], cex=cex_text, adj=c(0,0.5))
			if (!is.null(table_headings)) {
				text(x=newcol_pos, y=tabletop - 0.5*diff(table_rows_y[1:2]), labels=table_headings[ii], adj=c(0,0), font=2, cex=cex_text)
				# text(x=newcol_pos, y=tabletop + line_spacing*(strheight("A\nA", cex=cex_text) - 2*strheight("A", cex=cex_text)) + 0.5*strheight("A", cex=cex_text), labels=table_headings[ii], adj=c(0,0), font=2, cex=cex_text)
				newcol_pos <- newcol_pos + max(c(sapply(table_dat[,table_cols[ii]], strwidth, cex=cex_text), strwidth(table_headings[ii], cex=cex_text, font=2))) + colspacing
			} else {
				newcol_pos <- newcol_pos + max(sapply(table_dat[,table_cols[ii]], strwidth, cex=cex_text)) + colspacing
			}
				
		}
		
				
	}

		
	closeFile(type)
	
}


