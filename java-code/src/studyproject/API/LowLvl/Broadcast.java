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
