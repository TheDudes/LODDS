package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import org.junit.Test;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.BroadcastSenderThread;
import studyproject.API.Lvl.Mid.Lodds.Lodds;

public class BroadcastTest {

	/**
	 * Note: This test does not test if other clients can receive the broadcasts
	 * I also dont know of a feasible way to do so
	 * 
	 * The Interface is also really hard to determine automatically, this
	 * might cause an error
	 */
	@Test
	public void testBroadcasting(){
		String username = "junit";
		ArrayList<String> networkAddresses = new ArrayList<String>();
		Broadcast.getNetworkAddresses(networkAddresses);
		StringBuilder networkAddress = new StringBuilder();
		String chosenInterface = null;
		for(String interfaceName: networkAddresses){
			Broadcast.getLocalIp(interfaceName, networkAddress);
			if(!networkAddress.toString().equals("127.0.0.1") && (networkAddress.toString().startsWith("192.168")
					|| networkAddress.toString().startsWith("10.")) || networkAddress.toString().startsWith("172.16")){
				chosenInterface = interfaceName;
			}
		}
		if(chosenInterface == null){
			System.err.println("Could not determine a suitable interface");
			fail();
		} else {
			Lodds loddsObject = new Lodds(chosenInterface, username);
			BroadcastSenderThread sender = new BroadcastSenderThread(loddsObject);
			sender.start();

			Broadcast.getLocalIp(chosenInterface, networkAddress);
			BroadcastInfo broadcastInfo = new BroadcastInfo();
			Broadcast.readAdvertise(networkAddress.toString(), broadcastInfo);
			assertEquals(broadcastInfo.name, username);

			//shut down the sender
			sender.interrupt();
		}
	}
}
