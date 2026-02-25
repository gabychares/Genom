#primero se genera el project, puede ser uno nuevo o uno existente pero que lo quieres convertir en un repositorio de github
#ESTO SOLO SE HACE UNA VEZ POR MÁQUINA
#crear un token de github:
usethis::create_github_token()
#credenciales y aquí es donde te pide el token
gitcreds::gitcreds_set()

#Configurar
usethis::use_git_config(
  user.name = "gabychares",
  user.email = "gchavez27@alumnos.uaq.mx"
)

#subir el repositorio al github, esto ya solo se agrega cuando quieres crear un repositorio nuevo, ya solo se corre esto
usethis::use_github()
