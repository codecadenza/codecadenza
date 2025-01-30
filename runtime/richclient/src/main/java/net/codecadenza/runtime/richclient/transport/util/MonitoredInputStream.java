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
package net.codecadenza.runtime.richclient.transport.util;

import java.io.IOException;
import java.io.InputStream;
import net.codecadenza.runtime.richclient.transport.TransportDataDownloadListener;

/**
 * <p>
 * Specialization of an {@link InputStream} in order to monitor the download
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MonitoredInputStream extends InputStream {
	private static final int BYTE_THRESHOLD = 1024;

	private final InputStream target;
	private final TransportDataDownloadListener listener;
	private int totalBytesProcessed;
	private long downloadStart;
	private boolean started;
	private int currentBufferSize;

	/**
	 * Constructor
	 * @param target the input stream to be monitored
	 * @param listener the listener that should be notified
	 */
	public MonitoredInputStream(InputStream target, TransportDataDownloadListener listener) {
		this.target = target;
		this.listener = listener;
		this.listener.startDownload();
	}

	/**
	 * Notify the listener about the download progress
	 * @param bytesProcessed the current number of bytes that have been processed
	 */
	private void fireByteReceiveEvent(int bytesProcessed) {
		currentBufferSize += bytesProcessed;

		if (currentBufferSize >= BYTE_THRESHOLD) {
			listener.bytesReceived(totalBytesProcessed);
			currentBufferSize = 0;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException {
		if (!started) {
			started = true;
			this.downloadStart = System.currentTimeMillis();
		}

		totalBytesProcessed++;

		fireByteReceiveEvent(1);

		return target.read();
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.InputStream#close()
	 */
	@Override
	public void close() throws IOException {
		target.close();
		listener.downloadFinished((System.currentTimeMillis() - downloadStart));
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		if (!started) {
			started = true;
			this.downloadStart = System.currentTimeMillis();
		}

		totalBytesProcessed += (len - off);

		fireByteReceiveEvent(len - off);

		return target.read(b, off, len);
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.InputStream#read(byte[])
	 */
	@Override
	public int read(byte[] b) throws IOException {
		if (!started) {
			started = true;
			this.downloadStart = System.currentTimeMillis();
		}

		totalBytesProcessed += b.length;

		fireByteReceiveEvent(b.length);

		return target.read(b);
	}

}
