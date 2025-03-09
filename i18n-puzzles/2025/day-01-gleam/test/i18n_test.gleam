import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

pub fn bytes_and_chars_test() {
  "néztek bele az „ártatlan lapocskába“, mint ahogy belenézetlen mondták ki rá a halálos itéletet a sajtó csupa 20–30 éves birái s egyben hóhérai."
  |> i18n.bytes_and_chars
  |> should.equal(#(162, 143))

  "livres, et la Columbiad Rodman ne dépense que cent soixante livres de poudre pour envoyer à six milles son boulet d'une demi-tonne.  Ces"
  |> i18n.bytes_and_chars
  |> should.equal(#(138, 136))

  "Люди должны были тамъ и сямъ жить въ палаткахъ, да и мы не были помѣщены въ посольскомъ дворѣ, который также сгорѣлъ, а въ двухъ деревянныхъ"
  |> i18n.bytes_and_chars
  |> should.equal(#(253, 140))

  "Han hade icke träffat Märta sedan Arvidsons middag, och det hade gått nära en vecka sedan dess. Han hade dagligen promenerat på de gator, där"
  |> i18n.bytes_and_chars
  |> should.equal(#(147, 141))
}

pub fn cost_test() {
  #(162, 143) |> i18n.costs |> should.equal(0)
  #(138, 136) |> i18n.costs |> should.equal(13)
  #(253, 140) |> i18n.costs |> should.equal(7)
  #(147, 141) |> i18n.costs |> should.equal(11)
}
