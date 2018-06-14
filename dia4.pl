frase(Salida)-->gn(Suj,Persona,Numero),
				pa(A,PersonaA,Numero),
				primer_verbo(V,Persona,Numero),
				que,
				si,
				sujeto(N,Pe,Nu), % el verbo esta en la presona del sujeto si no en primera del singular
				verbo_aba(E),
				resto(R),
				{componer1(Salida,Suj,A,V,N,E,R)}.
				
que()-->[que].
si()-->[si].
si()-->[].

verbo_aba(R)-->[V],
				{name(V,Vs),
				es_terminacion(T,_,_),
				name(T,Ts),
				append(Rs,Ts,Vs),
				name(R,Rs)
				}.
				
componer1(Salida,S,A,V,N,E,R):-
							append(S,A,Aux1),
							append(Aux1,V,Aux2),
							append(Aux2,N,Aux3),
							append(Aux3,[:],Aux4),
							es_terminacion_ini(T,1,singular),
							name(T,Ts),
							name(E,Rs),
							append(Rs,Ts,Vs),
							name(Final,Vs),
							es_correcto(Final),
							append(Aux4,[Final],Aux5),
							append(Aux5,R,Salida).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
frase(Salida)--> 
				gn(SUJ, Persona, Numero),% el  sujeto de la frase principal
				pa( A,PersonaA, NumeroA),% me te le.....
                primer_verbo(V, Persona, Numero), %dijo /pregunto
				dp,								  % dos puntos
				sujeto(N,Pe,Nu),						  %  sujeto si tiene
				verbo(F,Pe,Nu),
				resto(R),                         % lista de el resto de las palabras
				   { componer(Salida, SUJ, A, V, N,F,R) }.
				  
gn([N], 3, Numero) --> [N],
                           {es_nombre(N, Numero) }.

gn([Pal], Persona, Numero) --> [Pal],
                                { es_pronombre(Pal, Persona, Numero) }.

pa([A],PersonaA,NumeroA) --> [A], 
				{es_atono(PersonaA, NumeroA, A)}.

primer_verbo([V], Persona, Numero) -->
                            [V],
                            { es_verbo(V, Persona, Numero)}.

resto([])-->[].
resto([X|Xs])-->[P],
				{pro_personal(P,X)},
				resto(Xs).

resto([X|Xs])-->[X],
				{\+pro_personal(X,_)},
				resto(Xs).
				
sujeto([N],3,J)-->[N], 
		{es_nombre(N,J)}.

sujeto([],_,_)-->[], 
		{}.
		
verbo(Raiz,Persona,Numero)-->[V],
			{   name(V, Vs),
				append(Rs, Ts, Vs),
                name(Raiz, Rs),
                name(Termin, Ts),
				es_terminacion_ini(Termin, Persona, Numero)
                

			}.
verbo(ser,3,singular)-->[es].
verbo(est,3,singular)-->[estoy].
			

dp --> [S],
		{es_dos(S)}.
		
		
cambio_me([],[]).
cambio_me([P|Aux],[X|Xs]):-
						name(te,Ts),
						name(X,Vs),
						append(Y,Ts,Vs),
						name(me,Ms),
						append(Y,Ms,A),
						name(P,A),
						cambio_me(Aux,Xs).

cambio_me([X|Aux],[X|Xs]):-cambio_me(Aux,Xs).
v(ver).



		
componer(Salida, GNs, A, V, N,ser,R) :-

                 append(GNs, A, Aux1), % María me
				 append(Aux1, V, Aux2),% dijo / pregunto
				 un_que(V,X),
				 append(Aux2, X, Aux3), % que o que si
				 append(Aux3, N, Aux4),% sujeto
				 
				 append(Aux4,[era],Aux5), % mete el verbo
				 cambio_me(Aux6,R),
				 append(Aux5,Aux6,Salida).
		
componer(Salida, GNs, A, V, N,F,R) :-

                 append(GNs, A, Aux1), % María me
				 append(Aux1, V, Aux2),% dijo / pregunto
				 un_que(V,X),
				 append(Aux2, X, Aux3), % que o que si
				 append(Aux3, N, Aux4),% sujeto
				 
				 es_aba(Terminacion),
				 name(Terminacion, Ts),%aba ASCII
				 name(F,Fs),		   %raiz en ASCII
				 append(Fs,Ts,Verbo),  % concatena en ASCII
				 name(Aux5,Verbo),		% palabra correspondiente con el codigo ASCII	
				 es_correcto(Verbo),
				 append(Aux4,[Aux5], Aux6), % mete el verbo
				 cambio_me(Aux7,R),
				 append(Aux6,Aux7,Salida).
				 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
				 
es_nombre(miguel, singular).
es_nombre(lucia, singular).
es_nombre(luis, singular).
es_nombre(maria, singular).
es_nombre(juan,singular).


pro_personal(mi,su).
pro_personal(esta,esa).

un_que([dijo],[que]).
un_que([pregunto],[que,si]).

es_aba(aba).

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

es_verbo(dije, 1, singular).
es_verbo(dijiste, 2, singular).
es_verbo(dijo, 3, singular).
es_verbo(dijimos, 1, plural).
es_verbo(dijisteis, 2, plural).
es_verbo(dijeron, 3, plural).
es_verbo(pregunto,3,singular).


es_terminacion(aba, 1, singular).
es_terminacion(abas, 2, singular).
es_terminacion(aba,  3, singular).
es_terminacion(abamos, 1, plural).
es_terminacion(abais,  2, plural).
es_terminacion(aban, 3, plural).


es_correcto(estoy).
es_correcto(necesito).

es_terminacion_ini(oy, 1, singular).
es_terminacion_ini(o, 1, singular).
es_terminacion_ini(as, 2, singular).
es_terminacion_ini(a,  3, singular).
es_terminacion_ini(amos, 1, plural).
es_terminacion_ini(ais,  2, plural).
es_terminacion_ini(an, 3, plural).

% verbo estar






es_dos(:).

