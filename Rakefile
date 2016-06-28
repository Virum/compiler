BUILD_DIR = '_build'
BUILD_DIR_REGEX = /^#{BUILD_DIR}\//

directory BUILD_DIR

task :default => :test

task :ppx => "#{BUILD_DIR}/ppx"

file "#{BUILD_DIR}/ppx" => BUILD_DIR do
  sh %{ocamlfind ocamlopt -predicates ppx_driver -o #{BUILD_DIR}/ppx -linkpkg \
       -package ppx_sexp_conv -package ppx_let ppx_driver_runner.cmxa}
end

task :test => :ppx do
  test_suite = BUILD_DIR + '/TestSuite.d.byte'
  ocamlbuild [test_suite]
  system test_suite
end

task :clean do
  sh "rm -rf #{BUILD_DIR} *.byte *.native"
end

# Build any `ocamlbuild` target, for example:
#   $ rake _build/test_gensym.d.byte
#   $ rake _build/mcs51/test_asm.native
rule BUILD_DIR_REGEX do |task|
  ocamlbuild [task.name.sub(BUILD_DIR_REGEX, '')]
end

task :build => :ppx do
  ocamlbuild ["#{BUILD_DIR}/Main.native"]
end

task :time => :ppx do
  sh 'rake clean && time rake test'
end

def ocamlbuild targets
  targets = targets.map {|target| target.sub BUILD_DIR_REGEX, ''}.join ' '
  sh %{ocamlbuild -quiet -build-dir #{BUILD_DIR} -I vendor \
       -cflags '-w @a-39-27-41-44-45-29-4 -annot -bin-annot' \
       -tag 'ppx(./ppx -as-ppx)' \
       -tag thread -use-ocamlfind \
       -use-menhir -menhir 'menhir --explain --strict' \
       -pkgs str,sexplib,core_kernel \
       #{targets}}
end
