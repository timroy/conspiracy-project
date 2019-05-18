pacman::p_load(tidyverse, haven)
data.og <- read_dta("./data_raw/NDI_Ukraine_Combined.dta")

data <- data.og %>% dplyr::select(c(## Outcome Variables
                                    # Why was Boris Nemstov killed (wave 1)
                                    BOROPP,	# Because of his overall opposition to the Russian gov
                                    BORTHRT,	#	Threatened to expose Russian involvement in Ukr conflict
                                    BORISLAM,	# Spoke out against Islamist attacks in Paris
                                    BORNEG,	#	To create a negative image of Putin/Russian gov
                                    BORDONTK,	#	Don't know who Boris Nemstov is
                                    BORUNCRT,	#	Don't know why he was killed
                                    BORDK,	# Why was Boris Nemstov killed	DK
                                    BORRA,	# Why was Boris Nemstov killed	RA
                                    ######## Waves 1, 4, 7, & 8
                                    MH17, # In July 2014, Malaysia Airlines Flight 17 crashed over Donetsk. In your opinion, what is the most likely reason for this?										
                                    ######## Wave 7
                                    INFRUSPR,	# Is Russian propaganda being spread in Ukraine? 	
                                    ######## Wave 8
                                    INFGENPR, #Is propaganda or disinformation being spread in Ukraine?
                                    INFPRORI,	# if yes, Who is responsible for spreading propaganda?	
                                    # What are your main sources of information for receiving news about 
                                    # politics and current events? Please tell me, which is your first main 
                                    # source of information and your second main source.				
                                    ######## Waves 1-2
                                    INFSOUFC, # First choice
                                    INFSOUSC, # Second choice
                                    ######## Wave 7
                                    INFSOMAI, # Main source
                                    ######## Waves 5 & 7
                                    INFSOTVF, #First choice TV
                                    INFSOTVS, #Second choice TV
                                    INFSOINF, #First choice Internet
                                    INFSOINS, #Second choice Internet
                                    INFSOSMF, #First choice Social media
                                    INFSOSMS, #Second choice Social media
                                    ####### Wave 5
                                    INFRAD01, # First choice Radio
                                    INFRAD02, # Second choice Radio
                                    INFSONEF, # First choice Newspaper
                                    INFSONES, # Second choice newspaper
                                    ######## Waves 1-6 & 8
                                    INFINTAC,	#How often do you access the internet for news?
                                    ######## Wave 7
                                    ACCSPUKR,	# The EU decided to fast track Ukraine’s accession to EU	Sample
                                    ACCSN, # ??????		
                                    ACCW0UKR, # Would you pass this story on?
                                    ACCIFTLD,	#	If you were told…Answer: 3P
                                    ACCW1UKR,	#	Would you pass this story on?
                                    ACCFRUKR,	#	If a friend or family member told you…Answer: 3P
                                    ACCW2UKR,	#	Would you pass this story on?
                                    ACCTVUKR,	#	If a TV news broadcaster reported…Answer: 3P
                                    ACCW3UKR,	#	Would you pass this story on?
                                    #
                                    BLCKDESP,	#Ukraine lifted its blockade on trade with Donbas	Sample
                                    BLCKDE,	# ?? NA (check this)
                                    BLCKDEW0,	#	Would you pass this story on?
                                    BLCIFTLD,	#	If you heard...
                                    BLCKDEW1,	#	Would you pass this story on?
                                    BLCKDDNR,	#	If a DNR spokesman denied this, would you believe it?
                                    BLCKDEW2,	#	Would you pass this story on?
                                    BLCKUGOV,	#	If the Ukrainian government denied this, would you believe it?
                                    BLCKDEW3,	#	Would you pass this story on?
                                    #
                                    BMHOSPSP,	# Ukrainian soldiers responsible for bombing Donbas hospital	Sample
                                    BMHOSP,	# ??	NA (check this)
                                    BMHFRSPRP,	#	If a friend says this is Russian propoganda, would you believe it?
                                    BMHTVRUS,	#	A Ukrainian TV broadcaster said this is Russian propoganda, would you believe it?
                                    BMHFCRUS,	#	A Ukrainian fact-checking website said this is Russian propaganda, would you believe it?
                                    #
                                    NOLANGSP,	# Russian language would no longer be taught in schools	Sample
                                    NORSLANG,	#	NA ?? (Check this)
                                    NOLIFTLD,	#	Would you believe it?
                                    NOLANGFC,	# A Ukrainian fact-checking website says this is untrue, would you believe it?
                                    NORLANGF,	#	A friend or family member says this is untrue, would you believe it?
                                    #
                                    EUHLDSP,	# EU put Ukraine's accession to the EU on hold	Sample
                                    EUHLDAC,	# NA ??? (check this)	
                                    EUHLDW0,	#	Would you pass this story on?
                                    EUHLDIFTLD,	#	Would you believe it?
                                    EUHLDW1,	#	Would you pass this story on?
                                    EUHLDACF,	#	If a friend or family member told you, would you believe it?
                                    EUHLDW2,	#	Would you pass this story on?
                                    EUHLDTV,	#	If a TV news broadcaster reported this, would you believe it?
                                    EUHLDW3,	#	Would you pass this story on?
                                    # 
                                    HRABSESP,	# There was an increase in human rights abuses in Donbas	Sample
                                    HRABUSES,	# NA ?? (check this)	
                                    HRIFTLDABS,	# Would you believe it?
                                    HRIFRUSPAR,	#	If the Russian parliament reported this, would you believe it?
                                    HRIFEURPAR,	#	If the European parliament reported this, would you believe it?
                                    # 
                                    LTNGSP, # Shop assistant said there's increased looting in Donbas
                                    LTDNETSK, # NA
                                    LTIFTLD, # Would you believe it?
                                    LTREBLS, # If there's looting by rebels (opolchenie) in Donetsk?
                                    LTFGHT, # If there's looting by fighters (boeviki) in Donetsk?
                                    #
                                    ### Explanatory Variables ###
                                    #
                                    DEMLEVEL, # Democracy worse, same, or improving?
                                    DEMLVLYA, # Democracy under Yanukovych
                                    DEMLVLPO, # Democracy under Poroshenko
                                    # Anomia - Wave 3,4, & 6
                                    LSFALLEN, # On average, living standards have fallen in Ukraine since 20 Who do you most blame for this?								
                                    # Wave 1-4, 6, & 8
                                    # In your opinion are any of the following interested in hearing your opinions?						
                                    HEARORD, # Order
                                    HEARPRES, # President
                                    HEARNATG, # National government
                                    HEARMEMP, # Member of Parliament
                                    HEARGOVE, # Governor
                                    HEARMAYR, # Mayor
                                    HEARLCON, # Local councilor
                                    HEARPOLP, # Political Party
                                    # Happiness - Wave 7
                                    LSCONTNT, # To what extent do you feel content about your situation in life at the moment?							
                                    LSIMPRVE, # To what extent, if at all, do you feel you are able to improve your situation in life?							
                                    # Waves 1-4, 6-8
                                    LSNEXTGN, # Overall, do you expect the next generation to be …?
                                    # Wave 2 
                                    INVLVPOL # How important is it for you to be involved in political life?					
                                    # Attitudestowards political figures? (Which ones to choose)
                                    ))
                                    