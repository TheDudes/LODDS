package studyproject.API.LowLvl;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Core.Utils;

public class Broadcast {
	
	private static final int PORT = 9002;
	private static final int PORT_SENDING = 9003;
	private static final int BUFFERSIZE = 2048;

	/**
	 * retrieves a list with all network addresses the local machine has
	 * 
	 * @param networkAddresses
	 * 			the list in which the addresses are put into
	 * 
	 * @return
	 * 			a list with all network addresses the local machine has
	 */
	public static int getNetworkAddresses(ArrayList<String> networkAddresses){
		try{
			Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
			while(networkInterfaces.hasMoreElements()){
				Enumeration<InetAddress> inetAddresses = networkInterfaces.nextElement().getInetAddresses();
				while(inetAddresses.hasMoreElements()){
					networkAddresses.add(inetAddresses.nextElement().getHostAddress());
				}
			}
		} catch(SocketException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}
	
	/**
	 * sends out an UDP broadcast to all machines in the local network. This broadcast contains the
	 * network address of the current machine, the port other machines should contact it from, the timestamp
	 * of the last change in the tracked filesystem and the name under which this machine shall be displayed
	 * 
	 * @param broadcastAddress
	 * 			the address the UDP socket has to broadcast to
	 * 
	 * @param networkAddress
	 * 			the network address of this machine (TCP address)
	 * 
	 * @param ipPort
	 * 			the port other machines should contact this machine from
	 * 
	 * @param timestamp
	 * 			the time of the last change in the tracked filesystem of this machine
	 * 
	 * @param name
	 * 			the name that other machines should display this machine as
	 * 
	 * @return
	 * 			0 or an error code
	 */
	public static int sendAdvertise(String broadcastAddress, String networkAddress, int ipPort, long timestamp, String name){
	    DatagramPacket packet;
	    try(DatagramSocket socket = new DatagramSocket(PORT_SENDING, InetAddress.getByName(networkAddress))){
	    	socket.setBroadcast(true);
	    	byte[] packetContents = (networkAddress + " " + ipPort + " " + timestamp + " " + name + "\n").getBytes();
	    	packet = new DatagramPacket(packetContents, packetContents.length,
	    			InetAddress.getByName(broadcastAddress), PORT);
	    	socket.send(packet);
	    	//socket.disconnect();
	    } catch(IOException e){
	    	//TODO real error codes
	    	return -1;
	    }
	    return 0;
	}
	
	
	/**
	 * listens on given UPD address for the next UDP packet and then puts the given information in the
	 * broadcastInfo if the given data matches the specification
	 * 
	 * @param broadcastAddress
	 * 			the address to listen to
	 * 
	 * @param broadcastInfo
	 * 			the broadcastInfo to put the read info in
	 * 
	 * @return
	 * 			0 or an error value
	 */	
	public static int readAdvertise(String broadcastAddress, BroadcastInfo broadcastInfo){
		try(DatagramSocket socket = new DatagramSocket(PORT, InetAddress.getByName(broadcastAddress))){
			DatagramPacket packet = new DatagramPacket(new byte[BUFFERSIZE], BUFFERSIZE);
			socket.receive(packet);
			int index = 0;
			while(packet.getData()[index] != '\n'){
				index++;
				if(!(index < BUFFERSIZE)){
					return -2;
				}
			}
			String packetContents = new String(Utils.getBytesFromTo(packet.getData(), 0, index), "UTF-8");
			String[] packetParts = packetContents.split(" ");
			if(packetParts.length != 4){
				//TODO real error codes
				return -2;
			}
			broadcastInfo.networkAddress = packetParts[0];
			broadcastInfo.ipPort = Integer.parseInt(packetParts[1]);
			broadcastInfo.timestamp = Long.parseLong(packetParts[2]);
			broadcastInfo.name = packetParts[3];
		} catch(IOException e){
			//TODO real error codes
			return -1;
		}
		return 0;
	}
}
