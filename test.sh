#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq

function fail {
    echo -e "FAIL: $1" >> /dev/stderr
    exit 1
}

#cabal build || fail "Failed to build"

#cabal test || fail "Failed to test"

EX="example-input.json"
ORDERED=$(cabal run -v0 order-deps < "$EX") ||
    fail "Failed to send '$EX' through order-deps"

echo "$ORDERED" | jq '.' > /dev/null ||
    fail "Couldn't parse output as JSON:\n$ORDERED"

# The elements of one SCC shouldn't depend on anything in a later SCC
DEPS=""
while read -r SCC
do
    # Check if anything in this SCC was needed by an earlier one
    TOOLATE=$(echo "$SCC" | jq --argfile deps <(echo "$DEPS") \
                               'map(select(. as $this | $deps | map(.name == $this.name and .module == $this.module and .package == $this.package) | any))')
    [[ "$(echo "$TOOLATE" | jq 'length')" -eq 0 ]] ||
        fail "Some dependencies showed up too late:\n$TOOLATE"

    DEPS=$(echo "$SCC" | jq --argfile deps <(echo "$DEPS") \
                            --argfile asts <(cat  "$EX")   \
                            '$deps + map(. as $this | $asts | .[] | if .name == $this.name and .module == $this.module and .package == $this.package then .dependencies else [] end | .[])')
done < <(echo "$ORDERED" | jq -c '.[]')
