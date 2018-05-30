frase(Salida) --> gn(SUJ, _G, Persona, Numero),
                  verbo(R, Tiempo, Persona, Numero, LPrep),
                  gn(CD, G, _P, N),
                  complemento(C, LPrep),
                  { componer(Salida, SUJ, R, Tiempo, CD, G, N, C) }.

gn([A, N], Genero, 3, Numero) --> [A, N],
                                  { es_articulo(A, Numero, Genero),
                                    es_nombre(N, Numero, Genero) }.

gn([P], _, Persona, Numero) --> [Pal],
                                { es_pronombre(Pal, Persona, Numero, P) }.

verbo(Rs, Tiempo, Persona, Numero, Lprep) -->
                            [V],
                            { name(V, Vs),
                              append(Rs, Ts, Vs),
                              name(Raiz, Rs),
                              es_raiz(Raiz, Lprep),
                              name(Termin, Ts),
                              es_terminacion(Termin, Tiempo, Persona, Numero)}.


complemento([P|GN], [P]) -->
                    [P],
                    { es_preposicion(P) },
                    gn(GN, _, _, _).
                             
% Si no hay complemento circunstancial
complemento([], _) --> [].

componer(Salida, GNs, Rs, Tiempo, CDs, G, N, Cs) :-
                 participio(Terminacion, G, N),
                 name(Terminacion, Ts),
                 append(Rs, Ts, Parts),
                 name(Part, Parts),
                 verbo_ser(Tiempo, N, Tiempo_ser),
                 append(CDs, [Tiempo_ser, Part, por], Aux1),
                 append(Aux1, GNs, Aux2),
                 append(Aux2, Cs, Salida).

participio(ada, fem, singular).
participio(ado, masc, singular).
participio(adas, fem, plural).
participio(ados, masc, plural).

verbo_ser(pasado, singular, fue).
verbo_ser(presente, singular, es).
verbo_ser(pasado, plural, fueron).
verbo_ser(presente, plural, son).

es_articulo(el, singular, masc).
es_articulo(la, singular, fem).
es_articulo(los, plural, masc).
es_articulo(las, plural, fem).
es_articulo(uno, singular, masc).
es_articulo(una, singular, fem).
es_articulo(unos, plural, masc).
es_articulo(unas, plural, fem).

es_nombre(libros, plural, masc).
es_nombre(ninos, plural, masc).
es_nombre(cuaderno, singular, masc).
es_nombre(decision, singular, fem).
es_nombre(nino, singular, masc).
es_nombre(flor, singular, fem).

es_pronombre(yo, 1, singular, mi, me).
es_pronombre(tu, 2, singular, ti, te).
es_pronombre(el, 3, singular, el, le).
es_pronombre(ella, 3, singular, ella, le).
es_pronombre(nosotros, 1, plural, nosotros, nos).
es_pronombre(nosotras, 1, plural, nosotras, nos).
es_pronombre(vosotros, 2, plural, vosotros, os).
es_pronombre(vosotras, 2, plural, vosotras, os).
es_pronombre(ellos, 3, plural, ellos, les).
es_pronombre(ellas, 3, plural, ellas, les).

% Una preposición permitida
es_raiz(pint, [en]).
es_raiz(habl, [con]).
% Ninguna preposición permitida
es_raiz(am, []).
es_raiz(tom, []).
% Cualquier preposición permitida
es_raiz(dibuj, [_]).

es_preposicion(en).
es_preposicion(con).

es_terminacion(o, presente, 1, singular).
es_terminacion(as, presente, 2, singular).
es_terminacion(a, presente, 3, singular).
es_terminacion(amos, presente, 1, plural).
es_terminacion(ais, presente, 2, plural).
es_terminacion(an, presente, 3, plural).

es_terminacion(�, pasado, 1, singular).
es_terminacion(aste, pasado, 2, singular).
es_terminacion(�, pasado, 3, singular).
es_terminacion(amos, pasado, 1, plural).
es_terminacion(asteis, pasado, 2, plural).
es_terminacion(aron, pasado, 3, plural).


/*
?- frase(Salida, [el, ni�o, dibuj�, una, flor], []).

Salida = [una, flor, fue, dibujada, por, el, ni�o] ;

No
?- frase(Salida, [el, ni�o, dibuj�, una, flor, en, el, cuaderno], []).

Salida = [una, flor, fue, dibujada, por, el, ni�o, en, el|...] ;

No
?- frase(Salida, [yo, tom�, la, decisi�n], []).

Salida = [la, decisi�n, fue, tomada, por, m�] ;

No
*/