{ mkDerivation, aeson, base, json-schema, postgresql-simple, stdenv
, text
}:
mkDerivation {
  pname = "postgresql-simple-errors-json";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ aeson base json-schema postgresql-simple text ];
  description = "Aeson and JSONSchema instances for postgresql-simple errors. *NB: Orphan Instances*";
  license = stdenv.lib.licenses.bsd3;
}
