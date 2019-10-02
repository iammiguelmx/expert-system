;;;=============================================================================
;;;
;;; Sistema experto para detectar la falla de un automovil
;;;
;;; Hecho por:
;;; Miguel Angel Camacho Campos <miguelangelcamachocampos@gmail.com>
;;; Analista:
;;; Celina Vianey
;;; Tester:
;;; Diana Soria
;;;		
;;; Este sistema experto estará diseñado para resolver una problemática con la que día a
;;;	día muchas personas en la sociedad tienen que lidiar y esto ha provocado grandes daños 
;;;	incluso muertes ya que en muchas ocasiones no se tiene la precaución de revisar el coche
;;;	antes de conducirlo, otras veces el coche falla a medio camino y no sabemos cómo resolver
;;;	esas fallas por que no se tiene ni idea de lo que está sucediendo.
;;;	
;;;	Para esto se desarrollará un sistema experto, considerando:
;;; 1.	Sistema de frenos
;;; 2.	Sistema eléctrico
;;; 3.	Sistema de luces
;;; 4.	Estado de gasolina
;;; 5.	Batería
;;; 6.	Calentamiento de motor
;;;	
;;;	Considerando al iniciar el sistema los siguientes datos de entrada:
;;;	DATOS AUTO:
;;;	•	Marca
;;;	•	Modelo
;;;	•	Año
;;;	•	Dueño
;;; Al finalizar el sistema experto, se despedirá de la siguiente manera: mensaje en el cuál 
;;;	se indica la falla del auto “falla”: Hasta pronto junto al nombre del dueño.
;;;
;;; Para ejecutar:
;;; (load fallasAutos.clp)
;;; (reset)
;;; (run)
;;;
;;; ============================================================================

(deffacts el-programa
	(versiones 1 2 3 4))

(deftemplate usuario
   (multislot nombre)
   (slot edad)
   (slot sexo))
   
   
;;;*****************************************************************************
;;;* MOSTRAR MENSAJE INICIAL Y SALIDA
;;;*****************************************************************************
(defrule datos_usuario ""
	(declare (salience 1))
	=>
	(printout t "Introduzca su nombre: ")
	(bind ?nombre_usuario (read))
	(printout t "Introduzca su edad: ")
	(bind ?edad_usuario (read))
	(printout t "Introduzca su sexo: ")
	(bind ?sexo_usuario (read))
	(printout t crlf crlf)
	(assert (usuario (nombre ?nombre_usuario)(edad ?edad_usuario)(sexo ?sexo_usuario))))
	
;;;*****************************************************************************
;;;   FUNCIONES PARA SACAR LA PREGUNTA Y QUE EL USUARIO LA RESPONDA
;;;*****************************************************************************

(deffunction pregunta (?pregunta $?valores)
	(printout t ?pregunta)
	(bind ?respuesta (read))
	(if (lexemep ?respuesta)
		then (bind ?respuesta (lowcase ?respuesta)))
	(while (not (member ?respuesta ?valores)) do
		(printout t ?pregunta)
		(bind ?respuesta (read))
		(if (lexemep ?respuesta)
			then (bind ?respuesta (lowcase ?respuesta))))
	?respuesta)

(deffunction si_or_no_p (?pregunta)
	(bind ?respuesta (pregunta ?pregunta si no s n))
	(if (or (eq ?respuesta si) (eq ?respuesta s))
		then si
		else no))
		
;;;*****************************************************************************
;;; DEFINCIONES DE LAS CLASES E INSTANCIAS INICIALES
;;;*****************************************************************************

(defclass FALLA
	(is-a USER)
	(slot motor   	(type SYMBOL))
	(slot luces   	(type SYMBOL))
	(slot frenos  	(type SYMBOL))
	(slot gasolina  (type SYMBOL))
	(slot bateria   (type SYMBOL))
	(slot electrico (type SYMBOL)))
	
;;; Definiendo los mensajes para leer los atributos
(defmessage-handler FALLA get_motor()
	(printout t "Falla en motor: " ?self:motor crlf))

(defmessage-handler FALLA get_luces()
	(printout t "Falla en luces: " ?self:luces crlf))	

(defmessage-handler FALLA get_frenos()
	(printout t "Falla en frenos: " ?self:frenos crlf))

(defmessage-handler FALLA get_gasolina()
	(printout t "Falla en gasolina: " ?self:gasolina crlf))

(defmessage-handler FALLA get_bateria()
	(printout t "Falla en bateria: " ?self:bateria crlf))

(defmessage-handler FALLA get_electrico()
	(printout t "Falla en electrico: " ?self:electrico crlf))
	
;;; Definiendo los mensajes para establecer los valores de los atributos

(defmessage-handler FALLA set_motor($?valores)
	(bind ?self:motor $?valores))
	
(defmessage-handler FALLA set_luces($?valores)
	(bind ?self:luces $?valores))
	
(defmessage-handler FALLA set_frenos($?valores)
	(bind ?self:frenos $?valores))
	
(defmessage-handler FALLA set_gasolina($?valores)
	(bind ?self:gasolina $?valores))
	
(defmessage-handler FALLA set_bateria($?valores)
	(bind ?self:bateria $?valores))
	
(defmessage-handler FALLA set_electrico($?valores)
	(bind ?self:electrico $?valores))
	
;;; Crear instancia de AUTO que será donde inferamos el conocimiento.
(definstances objeto_falla_actual
	(falla_actual of FALLA))
	
;;;*****************************************************************************
;;;* REGLAS PARA PREGUNTAR
;;;*****************************************************************************
(defrule status_motor ""
	(not (motor ?))
	=>
	(assert (motor (si_or_no_p "Sale humo del Motor (si/no)? "))))
	
(defrule status_luces ""
	(not (luces ?))
	=>
	(assert (luces (si_or_no_p "Las luces funcionan correctamente (si/no)? "))))
	
(defrule status_frenos ""
	(not (frenos ?))
	=>
	(assert (frenos (si_or_no_p "Los frenos funcionan bien (si/no)? "))))

(defrule status_gasolina ""
	(not (gasolina ?))
	=>
	(assert (gasolina (si_or_no_p "Tiene gasolina (si/no)? "))))
	
(defrule status_bateria ""
	(not (bateria ?))
	=>
	(assert (bateria (si_or_no_p "Esta cargada la bataría (si/no)? "))))
	
(defrule status_electrico ""
	(not (electrico ?))
	=>
	(assert (electrico (si_or_no_p "Funcionan ventajas (si/no)? "))))
	
;;;*****************************************************************************
;;; REGLAS PARA GUARDAR LAS COSAS EN EL OBJETO auto_actual Y CONDICIONES DE INTEGRIDAD
;;;*****************************************************************************
(defrule falla_motor ""
	(motor si)
	(luces no)
	(frenos no)
	(gasolina no)
	(bateria no)
	(electrico no)
	=>
	(send [falla_actual] set_luces luces)
	(send [falla_actual] set_frenos frenos)
	(send [falla_actual] set_gasolina gasolina)
	(send [falla_actual] set_bateria bateria)
	(send [falla_actual] set_electrico electrico)
	(send [falla_actual] set_motor motor))
	
(defrule falla_luces ""
	(motor no)
	(luces no)
	(frenos si)
	(gasolina no)
	(bateria no)
	(electrico no)
	=>
	(send [falla_actual] set_luces luces)
	(send [falla_actual] set_frenos frenos)
	(send [falla_actual] set_gasolina gasolina)
	(send [falla_actual] set_bateria bateria)
	(send [falla_actual] set_electrico electrico)
	(send [falla_actual] set_motor motor))
	
(defrule falla_frenos ""
	(motor no)
	(luces si)
	(frenos no)
	(gasolina no)
	(bateria no)
	(electrico no)
	=>
	(send [falla_actual] set_luces luces)
	(send [falla_actual] set_frenos frenos)
	(send [falla_actual] set_gasolina gasolina)
	(send [falla_actual] set_bateria bateria)
	(send [falla_actual] set_electrico electrico)
	(send [falla_actual] set_motor motor))
	
(defrule falla_gasolina ""
	(motor no)
	(luces no)
	(frenos no)
	(gasolina si)
	(bateria no)
	(electrico no)
	=>
	(send [falla_actual] set_luces luces)
	(send [falla_actual] set_frenos frenos)
	(send [falla_actual] set_gasolina gasolina)
	(send [falla_actual] set_bateria bateria)
	(send [falla_actual] set_electrico electrico)
	(send [falla_actual] set_motor motor))
	
;;;*****************************************************************************
;;;* REGLAS PARA DEDUCIR LA FALLA
;;;*****************************************************************************
(defrule falla_en_motor ""
	(motor si)
	(luces si)
	(frenos si)
	(gasolina si)
	(bateria si)
	(electrico si)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  Calentamiento de motor " crlf)
	(printout t "****************************************" crlf)
)

(defrule falla_en_luces ""
	(motor no)
	(luces no)
	(frenos si)
	(gasolina si)
	(bateria si)
	(electrico si)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  En el sistema de luces electricas  " crlf)
	(printout t "****************************************" crlf)
)

(defrule falla_en_frenos ""
	(motor no)
	(luces si)
	(frenos no)
	(gasolina si)
	(bateria si)
	(electrico si)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  Sistema de frenos  " crlf)
	(printout t "****************************************" crlf)
)

(defrule falla_en_gasolina ""
	(motor no)
	(luces si)
	(frenos si)
	(gasolina no)
	(bateria si)
	(electrico si)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  Falta gasolina  " crlf)
	(printout t "****************************************" crlf)
)

(defrule falla_en_bateria ""
	(motor no)
	(luces si)
	(frenos si)
	(gasolina si)
	(bateria no)
	(electrico si)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  Descarga de bateria  " crlf)
	(printout t "****************************************" crlf)
)

(defrule falla_si_electrico ""
	(motor no)
	(luces si)
	(frenos si)
	(gasolina si)
	(bateria si)
	(electrico no)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  Sistema electrico  " crlf)
	(printout t "****************************************" crlf)
)

(defrule buenas_condiciones ""
	(motor no)
	(luces si)
	(frenos si)
	(gasolina si)
	(bateria si)
	(electrico si)
	=>
	(printout t "****************************************" crlf)
	(printout t "*  Falla:  " crlf)
	(printout t "*  El auto esta en buenas condiciones  " crlf)
	(printout t "****************************************" crlf)
)


(defrule system-banner ""
(declare (salience 10))
=>
(printout t crlf crlf)
(printout t "****************************************" crlf)
(printout t "*  The expert car fault diagnosis system  *" crlf)
(printout t "****************************************" crlf)
(printout t crlf crlf)
)


; Regla que realiza la despedida del programa es la ultima en dispararse
(defrule print_falla ""
	(declare (salience -10))
	(usuario (nombre ?i))
	=>
	(printout t crlf crlf)
	(printout t "****************************************" crlf)
	(printout t "Hasta pronto " ?i "." crlf)
	(printout t "****************************************" crlf))
	

