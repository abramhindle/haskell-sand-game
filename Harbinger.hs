module Harbinger where
import Network.Socket as Net

data Harbinger = Harbinger { hname :: String,
                             hid :: String,
                             hdest :: String,
                             hsock :: Socket,
                             hserver :: SockAddr
                           }

-- Setup a UDP ready socket to send on 
makeHarbinger name id dest host port =
  let rhost = if (host == "") then "localhost" else rhost
      rport = if (port == 0) then 15011 else port
      rid = id
      rdest = dest
  in
  do
    rsock <- socket Net.AF_INET Datagram 0
    addr <- inet_addr rhost    
    let rserver = Net.SockAddrInet rport addr
    return $ Harbinger { hname = name, 
                         hid = rid, 
                         hdest = rdest, 
                         hsock = rsock, 
                         hserver = rserver }

makeDefaultHarbinger programName = makeHarbinger programName "" "" "" 0

harbingerSend harbinger msg =
    let msgtosend = concat [(hname harbinger) , "|" , 
                            (hid harbinger), "|",
                            (hdest harbinger), "|",
                            msg]
    in do
      sendTo (hsock harbinger) msgtosend (hserver harbinger)
