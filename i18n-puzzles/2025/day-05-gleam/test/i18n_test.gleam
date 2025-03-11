import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

const sample = " ⚘   ⚘ 
  ⸫   ⸫
🌲   💩  
     ⸫⸫
 🐇    💩
⸫    ⸫ 
⚘🌲 ⸫  🌲
⸫    🐕 
  ⚘  ⸫ 
⚘⸫⸫   ⸫
  ⚘⸫   
 💩  ⸫  
     ⸫⸫"

pub fn walk_test() {
  "1234\n4567\n890a\nbcde" |> i18n.walk |> should.equal("168d")
  sample |> i18n.walk |> should.equal(" ⸫💩⸫🐇  ⸫⚘  💩 ")
}

pub fn count_poo_test() {
  sample |> i18n.count_poo |> should.equal(2)
}
