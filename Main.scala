
case class Libro(titulo: String, autor: String, paginas: Int, anio: Int)
val catalogo: List[Libro] = List(
  Libro("Programación en Scala", "Ana Ruiz", 150, 2010),
  Libro("Fundamentos de FP", "Juan Pérez", 165, 2011),
  Libro("Algoritmos Modernos", "Carlos León", 180, 2012),
  Libro("Estructuras de Datos", "María Gómez", 195, 2013),
  Libro("Introducción a la Programación", "Luis Andrade", 210, 2014),
  Libro("Técnicas de Depuración", "Ana Ruiz", 225, 2015),
  Libro("Diseño de Sistemas", "Juan Pérez", 240, 2016),
  Libro("Patrones de Diseño", "Carlos León", 255, 2017),
  Libro("Aplicaciones Web", "María Gómez", 270, 2018),
  Libro("Cómputo en la Nube", "Luis Andrade", 285, 2019),
  Libro("Arquitectura de Software", "Ana Ruiz", 300, 2015),
  Libro("Bases de Datos", "Juan Pérez", 315, 2016),
  Libro("Microservicios", "Carlos León", 330, 2017),
  Libro("Concurrencia en Java", "María Gómez", 345, 2018),
  Libro("Pruebas Automatizadas", "Luis Andrade", 360, 2019),
  Libro("Seguridad Aplicada", "Ana Ruiz", 375, 2016),
  Libro("DevOps Práctico", "Juan Pérez", 390, 2017),
  Libro("Análisis de Datos", "Carlos León", 405, 2018),
  Libro("Machine Learning Básico", "María Gómez", 420, 2019),
  Libro("Redes de Computadores", "Luis Andrade", 435, 2019)
)
case class AutorInfo(autor: String, totalPaginas: Int, cantidadLibros: Int)
def autorMasProductivo(catalogo: List[Libro], paginasMinimas: Int, anioMinimo: Int): Option[AutorInfo] = {
  val librosFiltrados = catalogo.filter(libro => libro.paginas >= paginasMinimas && libro.anio >= anioMinimo)
  if (librosFiltrados.isEmpty) {
    return None
  }
  val autoresUnicos = librosFiltrados.map(_.autor).distinct
  val autorInfos = autoresUnicos.map { autor =>
    val librosDelAutor = librosFiltrados.filter(_.autor == autor)
    val totalLibros = librosDelAutor.length
    val totalPaginas = librosDelAutor.map(_.paginas).sum
    AutorInfo(autor, totalPaginas, totalLibros)
  }
  val autorMasProductivo = autorInfos.maxBy(_.totalPaginas)
  Some(autorMasProductivo)
}
