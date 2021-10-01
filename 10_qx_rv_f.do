
	**Calculo tasas brutas de mortalidad

	

	
	keep tm_sexo tm_fec_nac tm_fec_fall tm_fec_vig tm_cod_rel tm_fec_fin

	** Creacion de Variables **
	** --------------------- **

	** Fechas Exactas. Se pasa de edad en dias a edad en agnos
	local fechas = "nac fall vig fin "
	foreach k of local fechas {
		nsplit tm_fec_`k', digits(4 2 2) generate(`k'_agno `k'_mes `k'_dia)
	  	g `k'_exacta = (`k'_agno + (`k'_mes/12) + (`k'_dia/365.25))
		drop `k'_agno `k'_mes `k'_dia
	}
	
	**Parametros
	**Agno en que comienza la observacion
	global agno_c = 2010
	** Agno en que termina la observacion
	global agno_t = 2015


	** Edad Asegurada
	g IA = round(vig_exacta-nac_exacta,1)
	** Año de Vigencia
	g CYI = floor(tm_fec_vig/10000)
	** Fecha de Nacimiento
	g VYB = CYI - IA
	** Edad en que comienza la observacion
	g y = max($agno_c - VYB,IA)
	** Edad en la observacion sale del periodo observado
	g z = ($agno_t - VYB)
	** Edad exacta de muerte
	g theta = IA + fall_exacta - vig_exacta
	replace theta = 0 if tm_fec_fall==.
	** Edad exacta de fin de exposicion en caso de pasar a ser invalido
	g agno_renuncia=int(tm_fec_fin/10000)
	g gamma = max(agno_renuncia- VYB,IA)
	replace gamma = 0 if tm_fec_fin==99991231

	compress

	gen tabla=1
	
	** Condiciones de Expuesto y Fallecido
	forvalues i = 0/110 {
		g byte exp_`i' = 1*((y<`i'+1) & (z>=`i'+1) & (theta==0 | `i'<theta) & (gamma==0 | `i'<gamma) & (!inlist(tm_cod_rel,30,35) | `i'<24 | tabla==4))
		g byte fal_`i' = 1*(exp_`i'==1 & `i'<theta & theta<=`i'+1)
	} 
	
	
	** Calculo de Tasas Brutas **
	** ----------------------- **

	** Collapse para obtener la suma de expuestos y fallecidos por edad X
	collapse (sum) exp_* fal_*, by(tm_sexo)
	** Reshape para dejar la información como Tabla
	reshape long exp_ fal_, i(tm_sexo) j(edad)
	rename exp_ expuestos
	rename fal_ fallecidos

	compress

	** Tasa Bruta de Mortalidad qx
	gen qx = fallecidos/expuestos

	** Ordena Base de Datos
	compress
	sort tm_sexo edad, stable
	order tm_sexo edad expuestos fallecidos qx
	
	
	
	*********************
	drop ref
	gen ref=1*(tm_fec_fall!=.)
	order ref, after(tm_fec_fall)
drop ref