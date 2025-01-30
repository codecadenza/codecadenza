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
package net.codecadenza.runtime.richclient.transport.handler;

import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.FILE_TRANSPORT_HANDLER_ERR_FILE_NULL;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.FILE_TRANSPORT_HANDLER_ERR_MISSING_LOCAL_PATH;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.FILE_TRANSPORT_HANDLER_ERR_MISSING_SERVER_PATH;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.FILE_TRANSPORT_HANDLER_ERR_NON_EXISTING_FILE;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.FILE_TRANSPORT_HANDLER_ERR_NOT_A_FILE;
import static net.codecadenza.runtime.richclient.i18n.I18NRichClient.getTranslation;

import java.io.File;
import java.net.HttpURLConnection;
import java.net.URI;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.richclient.transport.util.DataDownloadThread;
import net.codecadenza.runtime.richclient.transport.util.DataUploadThread;
import net.codecadenza.runtime.stream.StreamWorker;
import net.codecadenza.runtime.transport.TransportConstants;

/**
 * <p>
 * Utility class to upload and download files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileTransportHandler {
	private static final String UPLOAD_CONTEXT = "/upload";
	private static final String DOWNLOAD_CONTEXT = "/download";
	private static final int STREAMING_BUFFER_SIZE = 1024;
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);

	private DataDownloadThread downloadThread;
	private DataUploadThread uploadThread;
	private final String url;
	private final boolean encrypt;
	private final boolean useChunkedStreamingMode;

	/**
	 * Constructor
	 * @param url
	 * @param encrypt
	 * @param useChunkedStreamingMode
	 */
	public FileTransportHandler(String url, boolean encrypt, boolean useChunkedStreamingMode) {
		this.url = url;
		this.encrypt = encrypt;
		this.useChunkedStreamingMode = useChunkedStreamingMode;
	}

	/**
	 * Upload a file to the server
	 * @param file
	 * @return the path of the file on the server
	 * @throws IllegalArgumentException if the given file either doesn't exist, or if it is a directory
	 * @throws Exception if the upload operation has failed
	 */
	public String uploadFile(File file) throws Exception {
		HttpURLConnection connection = null;
		final var streamWorker = new StreamWorker(SECRET);

		if (file == null)
			throw new IllegalArgumentException(getTranslation(FILE_TRANSPORT_HANDLER_ERR_FILE_NULL));

		if (!file.exists())
			throw new IllegalArgumentException(getTranslation(FILE_TRANSPORT_HANDLER_ERR_NON_EXISTING_FILE));

		if (file.isDirectory())
			throw new IllegalArgumentException(getTranslation(FILE_TRANSPORT_HANDLER_ERR_NOT_A_FILE));

		try {
			final URI targetURI = URI.create(url + UPLOAD_CONTEXT);

			connection = (HttpURLConnection) targetURI.toURL().openConnection();

			// Some HTTP servers doesn't support this special streaming mode!
			if (useChunkedStreamingMode)
				connection.setChunkedStreamingMode(STREAMING_BUFFER_SIZE);

			connection.setDoOutput(true);
			connection.setUseCaches(false);
			connection.addRequestProperty(TransportConstants.REQ_PROP_ENCRYPTION, Boolean.toString(encrypt));
			connection.addRequestProperty(TransportConstants.REQ_PROP_FILE_NAME, file.getName());
			connection.addRequestProperty(TransportConstants.CONTENT_TYPE_KEY, TransportConstants.CONTENT_TYPE_VALUE);

			// Write the content of the file to the output stream of the HTTP connection
			uploadThread = new DataUploadThread(connection, file, encrypt);
			uploadThread.start();

			// Wait until the upload thread has done his job!
			uploadThread.join();

			return (String) streamWorker.readObjectFromStream(connection.getInputStream(), encrypt);
		}
		finally {
			if (connection != null)
				connection.disconnect();
		}
	}

	/**
	 * Stop the upload operation
	 */
	public void stopUpload() {
		if (uploadThread != null)
			uploadThread.stopUpload();
	}

	/**
	 * Stop the download operation
	 */
	public void stopDownload() {
		if (downloadThread != null)
			downloadThread.stopDownload();
	}

	/**
	 * Download the file from the server
	 * @param clientPath
	 * @param serverPath
	 * @throws IllegalArgumentException if one of the provided path parameters is either null or empty
	 * @throws Exception if the download operation has failed
	 */
	public void downloadFile(String clientPath, String serverPath) throws Exception {
		HttpURLConnection connection = null;
		final var streamWorker = new StreamWorker(SECRET);

		if (clientPath == null || clientPath.isEmpty())
			throw new IllegalArgumentException(getTranslation(FILE_TRANSPORT_HANDLER_ERR_MISSING_LOCAL_PATH));

		if (serverPath == null || serverPath.isEmpty())
			throw new IllegalArgumentException(getTranslation(FILE_TRANSPORT_HANDLER_ERR_MISSING_SERVER_PATH));

		try {
			// Prepare the HTTP connection
			final var targetURL = URI.create(url + DOWNLOAD_CONTEXT);

			connection = (HttpURLConnection) targetURL.toURL().openConnection();

			// Some HTTP servers doesn't support this special streaming mode!
			if (useChunkedStreamingMode)
				connection.setChunkedStreamingMode(STREAMING_BUFFER_SIZE);

			connection.setDoOutput(true);
			connection.setUseCaches(false);
			connection.addRequestProperty(TransportConstants.REQ_PROP_ENCRYPTION, Boolean.toString(encrypt));
			connection.addRequestProperty(TransportConstants.CONTENT_TYPE_KEY, TransportConstants.CONTENT_TYPE_VALUE);

			// Send the path of the file
			streamWorker.writeObjectToStream(serverPath, connection.getOutputStream(), encrypt);

			// Read data from the input stream of the HTTP connection and write it to the selected file by using a new thread!
			downloadThread = new DataDownloadThread(connection, clientPath, encrypt);
			downloadThread.start();
			downloadThread.join();
		}
		finally {
			if (connection != null)
				connection.disconnect();
		}
	}

}
