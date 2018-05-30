frase(Salida)--> gn(SUJ, _G, Persona, Numero),
				  pa( A,PersonaA, NumeroA),
                  
				   { componer(Salida, SUJ, A) }.
				  
gn([N], _, 3, Numero) --> [N],
                                  {es_nombre(N, Numero) }.

gn([Pal], _, Persona, Numero) --> [Pal],
                                { es_pronombre(Pal, Persona, Numero) }.

pa([A],PersonaA,NumeroA) --> [A], 
				{es_atono(PersonaA, NumeroA, A)}.
				
componer(Salida, GNs, A) :-
                 append(GNs, A, Salida).
				
es_nombre(miguel, singular).
es_nombre(lucia, singular).
es_nombre(luis, singular).
es_nombre(maria, singular).


es_atono(1, singular, me).
es_atono(2, singular, te).
es_atono(3, singular, le).

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