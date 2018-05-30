frase(Salida)--> 
				gn(SUJ, Persona, Numero),% el  sujeto de la frase principal
				pa( A,PersonaA, NumeroA),% me te le.....
                verbo_decir(V, Persona, Numero),
				dp,
				com,
				comienzo(C,Persona,Numero),
				   { componer(Salida, SUJ, A, V) }.
				  
gn([N], 3, Numero) --> [N],
                           {es_nombre(N, Numero) }.

gn([Pal], Persona, Numero) --> [Pal],
                                { es_pronombre(Pal, Persona, Numero) }.

pa([A],PersonaA,NumeroA) --> [A], 
				{es_atono(PersonaA, NumeroA, A)}.
es_interrogacion
verbo_decir([V], Persona, Numero) -->
                            [V],
                            { es_verbo_decir(V, Persona, Numero)}.
							
comienzo(SR,Persona,Numero) --> [S, V], % Juan es -> Juan era
				{es_nombre(S, Numero),
				verbo(V,R,Persona,Numero),
				append(S, R, SR).
				}.
comienzo(R,Persona,Numero) --> [V],
				{es_verbo(V, ).}.
comienzo(SiR, PersonaA, NumeroA) --> [I, V],
				{es_interrogacion(I),
				es_verbo(V, ).}
				
verbo(V, Rs, Persona, Numero):-
				name(V, Vs),
                append(Rs, Ts, Vs),
                name(Raiz, Rs),
                name(Termin, Ts),
                es_terminacion_ini(Termin, Persona, Numero).           
dp --> [S],
		{es_dos(S)}.
					
com--> [C],
		{es_com(C)}.
		
componer(Salida, GNs, A, V) :-

                 append(GNs, A, Aux1), % María me
				 append(Aux1, V, Salida). % dijo / pregunto
				

				 
es_nombre(miguel, singular).
es_nombre(lucia, singular).
es_nombre(luis, singular).
es_nombre(maria, singular).


es_atono(1, singular, me).
es_atono(2, singular, te).
es_atono(3, singular, le).
es_atono(1, plural, nos).
es_atono(2, plural, os).
es_atono(3, plural, les).



es_pronombre(yo, 1, singular).
es_pronombre(tu, 2, singular).
es_pronombre(el, 3, singular).
es_pronombre(ella, 3, singular).
es_pronombre(nosotros, 1, plural).
es_pronombre(nosotras, 1, plural).
es_pronombre(vosotros, 2, plural).
es_pronombre(vosotras, 2, plural).
es_pronombre(ellos, 3, plural).
es_pronombre(ellas, 3, plural).

es_verbo_decir(dije, 1, singular).
es_verbo_decir(dijiste, 2, singular).
es_verbo_decir(dijo, 3, singular).
es_verbo_decir(dijimos, 1, plural).
es_verbo_decir(dijisteis, 2, plural).
es_verbo_decir(dijeron, 3, plural).

es_terminacion(aba, 1, singular).
es_terminacion(abas, 2, singular).
es_terminacion(aba,  3, singular).
es_terminacion(abamos, 1, plural).
es_terminacion(abais,  2, plural).
es_terminacion(aban, 3, plural).

es_terminacion_ini(o, 1, singular).
es_terminacion_ini(as, 2, singular).
es_terminacion_ini(a,  3, singular).
es_terminacion_ini(amos, 1, plural).
es_terminacion_ini(ais,  2, plural).
es_terminacion_ini(an, 3, plural).

verbo_ser(1, singular, era).
verbo_ser(2, singular, eras).
verbo_ser(3, singular, era).
verbo_ser(1, plural, eramos).
verbo_ser(2, plural, erais).
verbo_ser(3, plural, eran).
es_interrogacion(?).
es_dos(:).
es_com(comilla).

es_raiz(necesit, necesito).
es_raiz(necesit, necesitas).
es_raiz(necesit, necesita).
es_raiz(necesit, necesitamos).
es_raiz(necesit, necesitais).
es_raiz(necesit, necesitan).
es_raiz([],_,_,_).