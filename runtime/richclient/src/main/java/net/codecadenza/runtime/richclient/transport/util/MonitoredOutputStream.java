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
import java.io.OutputStream;
import net.codecadenza.runtime.richclient.transport.TransportDataUploadListener;

/**
 * <p>
 * Specialization of a {@link OutputStream} in order to monitor the upload
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MonitoredOutputStream extends OutputStream {
	private static final int BYTE_THRESHOLD = 1024;

	private final OutputStream target;
	private final TransportDataUploadListener listener;
	private int totalBytesProcessed;
	private long uploadStart;
	private boolean started;
	private int currentBufferSize;

	/**
	 * Constructor
	 * @param target the original output stream
	 * @param listener the listener to notify
	 */
	public MonitoredOutputStream(OutputStream target, TransportDataUploadListener listener) {
		this.target = target;
		this.listener = listener;
		this.listener.startUpload();
	}

	/**
	 * Notify the listener about the upload progress
	 * @param bytesProcessed the current number of bytes that have been processed
	 */
	private void fireByteSendEvent(int bytesProcessed) {
		currentBufferSize += bytesProcessed;

		if (currentBufferSize >= BYTE_THRESHOLD) {
			listener.bytesSent(totalBytesProcessed);
			currentBufferSize = 0;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.OutputStream#write(byte[], int, int)
	 */
	@Override
	public void write(byte[] b, int off, int len) throws IOException {
		if (!started) {
			started = true;
			this.uploadStart = System.currentTimeMillis();
		}

		target.write(b, off, len);

		totalBytesProcessed += (len - off);

		fireByteSendEvent(len - off);
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.OutputStream#write(byte[])
	 */
	@Override
	public void write(byte[] b) throws IOException {
		if (!started) {
			started = true;
			this.uploadStart = System.currentTimeMillis();
		}

		target.write(b);

		totalBytesProcessed += b.length;

		fireByteSendEvent(b.length);
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.OutputStream#write(int)
	 */
	@Override
	public void write(int b) throws IOException {
		if (!started) {
			started = true;
			this.uploadStart = System.currentTimeMillis();
		}

		target.write(b);

		totalBytesProcessed += 1;

		fireByteSendEvent(1);
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.OutputStream#close()
	 */
	@Override
	public void close() throws IOException {
		target.close();
		listener.uploadFinished((System.currentTimeMillis() - uploadStart));
	}

	/*
	 * (non-Javadoc)
	 * @see java.io.OutputStream#flush()
	 */
	@Override
	public void flush() throws IOException {
		target.flush();
	}

}
