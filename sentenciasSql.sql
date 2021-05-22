---1------
CREATE TABLE curso(
  codigo INT NOT NULL,
  nombre VARCHAR(45) NOT NULL,
  descripcion VARCHAR(45),
  turno VARCHAR(15) 
  )

---2-------
ALTER TABLE curso
	ADD cupo INT


----3------
-- A)
INSERT INTO curso VALUES (101, "Algoritmos","Algoritmos y Estructuras de Datos","Mañana",35);

-- B) 
INSERT INTO curso VALUES (102, "Matemática Discreta","","Tarde",30);
  

---4 y 5 son de probar ---

----6----
UPDATE curso 
SET cupo = 25


---7----
DELETE FROM curso
 WHERE nombre = "Algoritmos"


