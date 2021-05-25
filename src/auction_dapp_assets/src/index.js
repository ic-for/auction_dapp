import { Actor, HttpAgent } from '@dfinity/agent';
import { idlFactory as auction_dapp_idl, canisterId as auction_dapp_id } from 'dfx-generated/auction_dapp';

const agent = new HttpAgent();
const auction_dapp = Actor.createActor(auction_dapp_idl, { agent, canisterId: auction_dapp_id });

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  const greeting = await auction_dapp.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
