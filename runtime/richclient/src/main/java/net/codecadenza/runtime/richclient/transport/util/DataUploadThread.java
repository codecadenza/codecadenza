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

import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DATA_UPLOAD_THREAD_ERR_FILE_NOT_EXISTS;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DATA_UPLOAD_THREAD_ERR_NO_FILE;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.DATA_UPLOAD_THREAD_ERR_NO_HTTP_CON;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.getTranslation;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.invoke.MethodHandles;
import java.net.HttpURLConnection;
import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import net.codecadenza.runtime.crypto.CipherFactory;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Thread to upload a file to a HTTP server
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataUploadThread extends Thread {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final int DEFAULT_BUFFER_SIZE = 1024;
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);

	private final int bufferSize;
	private Cipher encryptionCipher;
	private final long totalBytesToProcess;
	private OutputStream uploadStream;
	private final File inputFile;
	private final boolean encryptOutput;
	private boolean stopUpload;

	/**
	 * Constructor
	 * @param connection
	 * @param inputFile
	 * @param encryptOutput
	 * @param bufferSize
	 * @throws Exception if the cipher couldn't be initialized
	 */
	public DataUploadThread(HttpURLConnection connection, File inputFile, boolean encryptOutput, int bufferSize) throws Exception {
		if (connection == null)
			throw new IllegalStateException(getTranslation(DATA_UPLOAD_THREAD_ERR_NO_HTTP_CON));

		if (inputFile.isDirectory())
			throw new IllegalStateException(getTranslation(DATA_UPLOAD_THREAD_ERR_NO_FILE));

		if (!inputFile.exists())
			throw new IllegalStateException(getTranslation(DATA_UPLOAD_THREAD_ERR_FILE_NOT_EXISTS, inputFile.getName()));

		this.bufferSize = bufferSize;
		this.totalBytesToProcess = inputFile.length();
		this.uploadStream = connection.getOutputStream();
		this.encryptOutput = encryptOutput;
		this.inputFile = inputFile;

		if (encryptOutput)
			this.encryptionCipher = new CipherFactory(SECRET).getEncryptionCipher();
	}

	/**
	 * Constructor
	 * @param connection
	 * @param inputFile
	 * @param encryptOutput
	 * @throws Exception if the cipher couldn't be initialized
	 */
	public DataUploadThread(HttpURLConnection connection, File inputFile, boolean encryptOutput) throws Exception {
		this(connection, inputFile, encryptOutput, DEFAULT_BUFFER_SIZE);
	}

	/**
	 * Stop the download operation
	 */
	public void stopUpload() {
		stopUpload = true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {
		super.run();

		final var buf = new byte[bufferSize];
		long bytesProcessed = 0;
		double percentage = 0;
		double lastPercentageSent = 0;
		int numRead = 0;

		try (final var fin = new FileInputStream(inputFile)) {
			if (encryptOutput)
				uploadStream = new CipherOutputStream(uploadStream, encryptionCipher);

			while ((numRead = fin.read(buf)) >= 0 && !stopUpload) {
				// We save the number of the bytes that have been downloaded so far
				bytesProcessed += numRead;

				percentage = (bytesProcessed * 100) / (double) totalBytesToProcess;

				// We shouldn't fire an event for every single chunk!
				if (percentage >= (lastPercentageSent + 1)) {
					lastPercentageSent = percentage;
					DataProcessedEventController.fireEvent(percentage, bytesProcessed, totalBytesToProcess);
				}

				uploadStream.write(buf, 0, numRead);
			}
		}
		catch (final Exception e) {
			logger.error("Error while uploading file!", e);

			// We have to handle all checked exceptions and simply throw a runtime exception if something goes wrong!
			throw new RuntimeException(e.getMessage());
		}
		finally {
			try {
				if (uploadStream != null)
					uploadStream.close();
			}
			catch (final IOException e) {
				logger.warn("Could not close output stream!", e);
			}
		}
	}

}
