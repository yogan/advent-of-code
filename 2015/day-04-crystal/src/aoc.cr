require "digest/md5"

def brute_force(key : String, start : String) : Int
  (0..).each do |i|
    return i if Digest::MD5.hexdigest(key + i.to_s).starts_with? start
  end
end

def main()
  key = File.read("input.txt").lines[0]
  puts brute_force(key, "00000")
  puts brute_force(key, "000000")
end
