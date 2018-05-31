fun is_older (a,b) =
  case (a, b) of
       ((x, y, z), (u, v, w)) => if x < u then true else if 
                                    x = u andalso y < v then true else if
                                    y = v andalso z < w then true else false

fun number_in_month (a, b) = 
    let fun acc(date) =
        case date of
             (y, m, d) => if b = m then 1 else 0
    in
      case a of
           [] => 0
         | x::xs' => acc(x) + number_in_month(xs', b)
    end

fun number_in_months (a, b) =
  case b of
       [] => 0
     | m::[] => number_in_month(a, m)
     | m::ms' => number_in_month(a, m) + number_in_months(a, ms')

fun dates_in_month (dates, m) = 
  let fun check date =
        case date of
             (y, mm, d) => if mm = m then true else false
  in
    case dates of
         [] => []
       | x::xs' => if check x then x::(dates_in_month(xs', m)) else dates_in_month(xs', m)
  end

fun dates_in_months (dates, months) =
  case months of
       [] => []
     | m::[] => dates_in_month(dates, m)
     | m::ms' => dates_in_month(dates, m)@(dates_in_months(dates, ms'))

fun get_nth (strs, idx) =
  let fun check(s, e) =
        if s = e then true else false
  in
        case strs of
             str::[] => str
           | str::str' => if check(1, idx) then str else get_nth(str', idx-1)
  end   

fun date_to_string date =
  let val MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    case date of
         (a, b, c) => get_nth(MONTHS, b)^" "^(Int.toString c)^", "^(Int.toString
         a)
  end


