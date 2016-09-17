package studyproject.API.Lvl.Low;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.regex.Pattern;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Core.Utils;

/**
 * Class can send an receive broadcasts that are in
 * accordance with the LODDS specification
 * Can also display all currently active network devices of the local machine
 * @author Michael
 *
 */
public class Broadcast {
	
	private static final int DEFAULT_PORT = 9002;
	private static final int DEFAULT_PORT_SENDING = 9003;
	private static final int DEFAULT_BUFFERSIZE = 2048;
	private static final String ADVERTISE_BROADCAST_REGEX = "(\\d{1,3}\\.){3}\\d{1,3} \\d{1,5} \\d{1,19} \\d{1,19} \\S*";
	
	private static int broadcastPort = DEFAULT_PORT;
	private static int outgoingPort = DEFAULT_PORT_SENDING;
	private static int buffersize = DEFAULT_BUFFERSIZE;

	/**
	 * retrieves a list with all network devices the local machine has that are running and not virtual
	 * 
	 * @param networkAddresses
	 * 			the list in which the addresses are put into
	 * 
	 * @return
	 * 			a list with all network addresses the local machine has that are running and not virtual
	 */
	public static int getNetworkAddresses(ArrayList<String> networkAddresses){
		try{
			Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
			while(networkInterfaces.hasMoreElements()){
				NetworkInterface networkInterface = networkInterfaces.nextElement();
				if(networkInterface.isUp() && !networkInterface.isVirtual()){
					networkAddresses.add(networkInterface.getDisplayName());
				}
				
			}
		} catch(SocketException e){
			return 1;
		} catch(NoSuchElementException f){
			//TODO real error codes
			return -4;
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
	 * @param load
	 * 			how many bytes the client still has to send to other clients as part of a file transfer
	 * 
	 * @param name
	 * 			the name that other machines should display this machine as
	 * 
	 * @return
	 * 			0 or an error code
	 */
	public static int sendAdvertise(String broadcastAddress, String networkAddress, int ipPort, long timestamp, long load, String name){
	    DatagramPacket packet;
	    try(DatagramSocket socket = new DatagramSocket(outgoingPort, InetAddress.getByName(networkAddress))){
	    	socket.setBroadcast(true);
	    	byte[] packetContents = (networkAddress + " " + ipPort + " " + timestamp + " " + load + " " + name + "\n").getBytes();
	    	packet = new DatagramPacket(packetContents, packetContents.length,
	    			InetAddress.getByName(broadcastAddress), broadcastPort);
	    	socket.send(packet);
	    	//socket.disconnect();
	    } catch(IOException e){
	    	return 1;
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
		try(DatagramSocket socket = new DatagramSocket(broadcastPort, InetAddress.getByName(broadcastAddress))){
			DatagramPacket packet = new DatagramPacket(new byte[buffersize], buffersize);
			socket.receive(packet);
			int index = 0;
			while(packet.getData()[index] != '\n'){
				index++;
				if(!(index < buffersize)){
					return -2;
				}
			}
			String packetContents = new String(Utils.getBytesFromTo(packet.getData(), 0, index), "UTF-8");
			if(!Pattern.matches(ADVERTISE_BROADCAST_REGEX, packetContents)){
				return 2;
			}
			String[] packetParts = packetContents.split(" ");
			broadcastInfo.networkAddress = packetParts[0];
			broadcastInfo.ipPort = Integer.parseInt(packetParts[1]);
			broadcastInfo.timestamp = Long.parseLong(packetParts[2]);
			broadcastInfo.load = Long.parseLong(packetParts[3]);
			String clientName = "";
			clientName += packetParts[4];
			for(int packetIndex = 5; packetIndex < packetParts.length; packetIndex++){
				clientName += " " + packetParts[packetIndex];
			}
			broadcastInfo.name = clientName;
		} catch(IOException e){
			return 1;
		} catch(NumberFormatException f){
			return 2;
		}
		return 0;
	}

	/**
	 * 
	 * @return
	 * 			the port the UDP packets are sent to
	 */
	public static int getBroadcastPort() {
		return broadcastPort;
	}

	/**
	 * 
	 * @param broadcastPort
	 * 			the port the UDP packets are sent to
	 */
	public static void setBroadcastPort(int broadcastPort) {
		Broadcast.broadcastPort = broadcastPort;
	}

	/**
	 * 
	 * @return
	 * 			the port the UDP socket is bound to locally
	 */
	public static int getOutgoingPort() {
		return outgoingPort;
	}

	/**
	 * 
	 * @param outgoingPort
	 * 			the port the UDP socket is bound to locally
	 */
	public static void setOutgoingPort(int outgoingPort) {
		Broadcast.outgoingPort = outgoingPort;
	}

	/**
	 * 
	 * @return
	 * 			the size of the buffer used to read data from the UDP packet
	 */
	public static int getBuffersize() {
		return buffersize;
	}

	/**
	 * 
	 * @param buffersize
	 * 			the size of the buffer used to read data from the UDP packet
	 */
	public static void setBuffersize(int buffersize) {
		Broadcast.buffersize = buffersize;
	}
	
	
}
