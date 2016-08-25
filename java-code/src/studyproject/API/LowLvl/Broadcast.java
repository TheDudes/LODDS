package studyproject.API.LowLvl;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;

public class Broadcast {
	
	private static int port = 9002;

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
	
	public static int sendBroadcast(String broadcastAddress, String networkAddress, int ipPort, long timestamp, String name){
	    DatagramPacket packet;
	    try(DatagramSocket socket = new DatagramSocket(port, InetAddress.getByName(broadcastAddress))){
	    	socket.setBroadcast(true);
	    	byte[] packetContents = (networkAddress + " " + ipPort + " " + timestamp + " " + name + "\n").getBytes();
	    	packet = new DatagramPacket(packetContents, 0, InetAddress.getByName(broadcastAddress), port);
	    	socket.send(packet);
	    } catch(IOException e){
	    	//TODO real error codes
	    	return -1;
	    }
	    return 0;
	}
	
}
