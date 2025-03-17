import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

pub fn find_matching_rotation_test() {
  "σζμ γ' ωοωλδθαξλδμξρ οπξρδυζ οξκτλζσθρ Ξγτρρδτρ."
  |> i18n.find_matching_rotation
  |> should.equal(1)

  "αφτ κ' λαλψφτ ωπφχλρφτ δξησηρζαλψφτ φελο, Φκβωωλβ."
  |> i18n.find_matching_rotation
  |> should.equal(18)

  "γ βρφαγζ ωνψν ωγφ πγχρρφ δρδαθωραγζ ρφανφ."
  |> i18n.find_matching_rotation
  |> should.equal(0)
}

pub fn rotate_test() {
  "σζμ γ' ωοωλδθαξλδμξρ οπξρδυζ οξκτλζσθρ Ξγτρρδτρ."
  |> i18n.rotate(1)
  |> should.equal("την δ' απαμειβομενος προσεφη πολυμητις Οδυσσευς.")
}
