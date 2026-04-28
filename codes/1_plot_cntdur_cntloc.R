# compare the proportion of contacts made in other places (neither at home, at school nor at workplaces)


# compositions of categories in others
loc_otherplace_vec = c("transport", "religious", "sports", "outside", "shopping", "fnb", "otherplace")
loc_vec = c("home", "school", "work", loc_otherplace_vec)

cnt_loc_otherplace_vec = paste0("cnt_", loc_otherplace_vec)
cnt_loc_vec = paste0("cnt_", loc_vec)
match(cnt_loc_vec, colnames(chk_contact_base)) # check variable names
table_cnt_loc_wothers = do.call(cbind, sapply(c(cnt_loc_vec, "cnt_others"), simplify=FALSE, function(xx_cnt_loc) table(chk_contact_base[, xx_cnt_loc])));
table_cnt_loc = table_cnt_loc_wothers[, grep("cnt_others", colnames(table_cnt_loc_wothers), invert=TRUE)]
table_cnt_loc_byothers = sapply(cnt_loc_vec, simplify=FALSE, function(xx_cnt_loc) table(chk_contact_base[, c(xx_cnt_loc, "cnt_others")]));

# divide part and contact by phase (4 phases)
# phases defined in 5a_xx.R
num_phases = 4
date_byphase = as.Date(c("2021-09-01", "2022-01-07", "2022-04-21", "2023-03-01", "2024-01-01"))
labels_date_byphase = as.Date(c("2021-09-17", "2022-01-07", "2022-04-21", "2023-03-01", "2024-01-01"))
num_date_byphase = length(date_byphase)-1;

if (TRUE){ # iphase==5 for all, regardless of the phase of the study
	num_date_byphase = num_date_byphase + 1 
}

num_subgp = 3;
out_list_phase_template = setNames(rep(rep(list(), num_date_byphase, num_subgp)), c("all", "genpop", "spg"))


chk_part_base_phase = rep(list(), num_phases)
chk_contact_base_phase = chk_part_base_phase

for (iphase in 1:num_date_byphase){
	if (iphase <=4 ){
		irow_TT = (chk_part_base$date >= date_byphase[iphase] & chk_part_base$date < date_byphase[iphase+1] ); 
	} else if (iphase ==5) {
		irow_TT = rep(TRUE, nrow(chk_part_base)) # all participants
	}
	# all participants
	chk_part_TT = chk_part_base[irow_TT,];
	chk_part_base_phase[["all"]][[iphase]] = chk_part_TT
	chk_contact_TT = chk_contact_base[chk_contact_base$part_id %in% chk_part_TT$part_id,]
	chk_contact_TT$cntdur_abv15min = 15*(chk_contact_TT$duration_multi%in%c(3,4,5))
	chk_contact_base_phase[["all"]][[iphase]] = chk_contact_TT

	# gen pop only
	irow_TT2 = (irow_TT & chk_part_base$part_specialgroups_YN==0 ); # among participants not in the special groups 
	chk_part_TT = chk_part_base[irow_TT2,];
	chk_part_base_phase[["genpop"]][[iphase]] = chk_part_TT
	chk_contact_TT = chk_contact_base[chk_contact_base$part_id %in% chk_part_TT$part_id,]
	chk_contact_TT$cntdur_abv15min = 15*(chk_contact_TT$duration_multi%in%c(3,4,5))
	chk_contact_base_phase[["genpop"]][[iphase]] = chk_contact_TT

	# spg only
	irow_TT2 = (irow_TT & !(chk_part_base$part_specialgroups %in% 997) ); # among participants in the special groups 
	chk_part_TT = chk_part_base[irow_TT2,];
	chk_part_base_phase[["spg"]][[iphase]] = chk_part_TT
	chk_contact_TT = chk_contact_base[chk_contact_base$part_id %in% chk_part_TT$part_id,]
	chk_contact_TT$cntdur_abv15min = 15*(chk_contact_TT$duration_multi%in%c(3,4,5))
	chk_contact_base_phase[["spg"]][[iphase]] = chk_contact_TT
} # for- iphase


# counts
out_cnt_loc_phase = out_list_phase_template
summ_cnt_loc_phase = out_list_phase_template

# by contact duration (above or below 15 minutes)
out_cnt_loc_2cntdur_phase = out_list_phase_template
summ_cnt_loc_2cntdur_phase = out_list_phase_template

out_cnt_loc_5cntdur_phase = out_list_phase_template
summ_cnt_loc_5cntdur_phase = out_list_phase_template

out_cnt_loc_phycnt_phase = out_list_phase_template
summ_cnt_loc_phycnt_phase = out_list_phase_template


for (ii_subgp in 1:3){

	for (iphase in 1:num_date_byphase){

		chk_contact_TT = chk_contact_base_phase[[ii_subgp]][[iphase]]
		
		
		# all locations
		table_cnt_loc_TT = do.call(cbind, sapply(c(cnt_loc_vec, "cnt_others"), simplify=FALSE, function(xx_cnt_loc) table(chk_contact_TT[, xx_cnt_loc])));
		summout_TT = c(n=sum(table_cnt_loc_TT[,1]),
			prop_all = round(100*prop.table(table_cnt_loc_TT["1",cnt_loc_vec]),1),
			prop_others = round(100*prop.table(table_cnt_loc_TT["1",cnt_loc_otherplace_vec]),1)
		)
		
		out_cnt_loc_phase[[ii_subgp]][[iphase]] = table_cnt_loc_TT
		summ_cnt_loc_phase[[ii_subgp]][[iphase]] = summout_TT
		

		# contact duration
		table_cnt_loc_2cntdur_TT = sapply(c(cnt_loc_vec, "cnt_others"), simplify=FALSE, function(xx_cnt_loc) table(chk_contact_TT[, c(xx_cnt_loc, "cntdur_abv15min")])["1",]);
		table_cnt_loc_2cntdur_TT = do.call(cbind, table_cnt_loc_2cntdur_TT)
		summout_2cntdur_TT = prop.table(table_cnt_loc_2cntdur_TT,2)

		table_cnt_loc_5cntdur_TT = sapply(c(cnt_loc_vec, "cnt_others"), simplify=FALSE, function(xx_cnt_loc) table(chk_contact_TT[, c(xx_cnt_loc, "duration_multi")])["1",]);
		table_cnt_loc_5cntdur_TT = do.call(cbind, table_cnt_loc_5cntdur_TT)
		summout_5cntdur_TT = prop.table(table_cnt_loc_5cntdur_TT,2)

		out_cnt_loc_2cntdur_phase[[ii_subgp]][[iphase]] = table_cnt_loc_2cntdur_TT
		summ_cnt_loc_2cntdur_phase[[ii_subgp]][[iphase]] = summout_2cntdur_TT
		out_cnt_loc_5cntdur_phase[[ii_subgp]][[iphase]] = table_cnt_loc_5cntdur_TT
		summ_cnt_loc_5cntdur_phase[[ii_subgp]][[iphase]] = summout_5cntdur_TT
		

		# physical / non-physical contact
		table_cnt_loc_phycnt_TT = sapply(c(cnt_loc_vec, "cnt_others"), simplify=FALSE, function(xx_cnt_loc) table(chk_contact_TT[, c(xx_cnt_loc, "phys_contact")])["1",]);
		table_cnt_loc_phycnt_TT = do.call(cbind, table_cnt_loc_phycnt_TT)
		summout_phycnt_TT = prop.table(table_cnt_loc_phycnt_TT,2)

		out_cnt_loc_phycnt_phase[[ii_subgp]][[iphase]] = table_cnt_loc_phycnt_TT
		summ_cnt_loc_phycnt_phase[[ii_subgp]][[iphase]] = summout_phycnt_TT	
	
	} # for- iphase
} # for- ii_subgp


summ_cnt_loc = lapply(summ_cnt_loc_phase, function(xx_summ) do.call(cbind, xx_summ))
y_summ = summ_cnt_loc[["genpop"]]


# barplot, by contact duration

x_label_cnt_loc = c("home"="Home", "school"="School", "work"="Work", "transport"="Public\ntransport", "religious"="Places of\nworship", "sports"="Sports\nfacilities", "outside"="Outdoor\nenvironments", "shopping"="Entertainment\nand retail\nvenues", "fnb"="Food and\nbeverage\nvenues", "otherplace"="Other\nlocations")

col_phases = c("blue", "red", "green3", "gold")
legend_text = c("Pre-fifth wave", "Fifth wave", "Post-fifth wave", "Post-pandemic")

labels_dur = c('<5 mins', '5-14 mins', '15-59 mins', '1-4 hrs', '>4 hrs')
num_dur_catg = 5;
COLS_cntdur = rev(ggpubr::get_palette(palette = "YlGn", num_dur_catg))

labels_phy = c("Physical", "Non-physical")
num_phy = 2
COLS_phycnt = rev(ggpubr::get_palette(palette = "YlGn", 3))


fname_pdf = sprintf("contact_location_byphase_cntdur_phycnt_%s.pdf", update_outputdate())
plotPDF_YN = TRUE

idx_loc_catg = 4:10 # number of column
num_loc_catg = length(idx_loc_catg)

if (plotPDF_YN){
    pdf(fname_pdf, width=6, height=5) # width=10 for 10 categories
} else{
    windows(width=12, height=4)
}

iphase_vec_plot = 1:5

for (iplot in 1:2){
	if (iplot==1){
		COLS = COLS_cntdur;
		leg_labels = labels_dur;
		num_catg = num_dur_catg;
	} else if (iplot==2){
		COLS = COLS_phycnt;
		leg_labels = labels_phy;
		num_catg = num_phy;
	}


for (ii_subgp in 2:3){
for (iphase in iphase_vec_plot){
	if (iphase !=5){
		title_text = legend_text[iphase]
	} else {
		title_text = "The entire study"
	}
	title_text = sprintf("%s (n=%s)", title_text, format(summ_cnt_loc[[ii_subgp]]["n", iphase], big.mark=","))
	title_text = paste(names(summ_cnt_loc)[ii_subgp], title_text, sep="-")

	if (iplot==1){
		proptable_TT = t(summ_cnt_loc_5cntdur_phase[[ii_subgp]][[iphase]][, idx_loc_catg])	
	} else if (iplot==2){
		proptable_TT = t(summ_cnt_loc_phycnt_phase[[ii_subgp]][[iphase]][, idx_loc_catg])	
	}

	xlim_plot = c(1,num_loc_catg) + 0.5*c(-1,1)
	plot(NA, xlim=c(1,num_loc_catg)+0.5*c(-1,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="", xaxs="i", yaxs="i")
	for (ii in 1:num_loc_catg){
		xx_prop = proptable_TT[ii, ]
		xx_prop_cumsum = c(0, cumsum(xx_prop))

		bar_width = 0.475;
		for (yy in 1:num_dur_catg){
			polygon(x=c(ii+bar_width*c(-1,1), rev(ii+bar_width*c(-1,1))), y=c(rep(xx_prop_cumsum[yy],2), rep(xx_prop_cumsum[yy+1],2)), col=COLS[yy], border=NA)
		}
	} # for- ii


	# legend
	legend("topright", legend=leg_labels, horiz=TRUE, col="blank", fill=COLS, cex=0.8, inset=c(0, -0.15), xpd=NA, bty="n", text.width=strwidth(leg_labels)/max(strwidth(leg_labels)), x.intersp=0.75)

	# axis
	text(x=1:num_loc_catg, y=-0.05, labels=x_label_cnt_loc[idx_loc_catg], xpd=TRUE, srt=0, cex=0.7, adj=c(0.5,1))
	mtext(side=1, text="Location of contacts", line=3.5, cex=1)
	
	ytick_major = seq(0,1, by=0.2)
	axis(side=2, at=ytick_major, labels=100*ytick_major, cex.axis=1.2, pos=0.4, las=1)
	mtext(side=2, text="Proportion (%)", line=3, cex=1)
	
	
	# title
	mtext(side=3, text=title_text, line=2, at=0, adj=0, cex=1)

} # for- iphase
} # for- ii_subgp
} # for- iplot
if (plotPDF_YN) { dev.off()}

