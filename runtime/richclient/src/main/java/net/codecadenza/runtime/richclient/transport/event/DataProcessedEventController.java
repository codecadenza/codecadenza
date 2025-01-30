/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.richclient.transport.event;

import java.util.ArrayList;

/**
 * <p>
 * Simple event controller for upload and download events
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataProcessedEventController {
	// List of registered listeners
	private static ArrayList<DataProcessedListener> listenerList = new ArrayList<>();

	/**
	 * Prevent instantiation
	 */
	private DataProcessedEventController() {

	}

	/**
	 * Add a listener
	 * @param listener the listener to be added
	 */
	public static synchronized void addListener(DataProcessedListener listener) {
		listenerList.add(listener);
	}

	/**
	 * @param listener
	 */
	public static synchronized void removeListener(DataProcessedListener listener) {
		listenerList.remove(listener);
	}

	/**
	 * Notify all listeners
	 * @param percentage
	 * @param numberOfBytes
	 * @param totalNumberOfBytes
	 */
	public static synchronized void fireEvent(double percentage, long numberOfBytes, long totalNumberOfBytes) {
		listenerList.forEach(listener -> listener.onPercentCompleted(percentage, numberOfBytes, totalNumberOfBytes));
	}

}
