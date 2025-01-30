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
package net.codecadenza.runtime.richclient.swing.file;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_FILE_DOWNLOAD_OPERATION_BYTES_PROCESSED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_ERROR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_FINISHED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_FILE_DOWNLOAD_OPERATION_STATUS_PREPARE_DOWNLOAD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import net.codecadenza.runtime.file.ByteConverter;
import net.codecadenza.runtime.richclient.swing.widget.StatusReceivable;
import net.codecadenza.runtime.richclient.transport.ServiceLocator;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * File download operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFileDownloadOperation implements DataProcessedListener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final StatusReceivable status;

	/**
	 * Constructor
	 * @param status
	 */
	protected AbstractFileDownloadOperation(StatusReceivable status) {
		this.status = status;

		// Register this object to be a listener for appropriate events!
		DataProcessedEventController.addListener(this);
	}

	/**
	 * Abstract method to be implemented by using a specific download method
	 * @return the fully qualified path name of the file to download
	 * @throws Exception if the preparation of the download has failed
	 */
	public abstract String prepareDownload() throws Exception;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.transport.event.DataProcessedListener#onPercentCompleted(double, long, long)
	 */
	@Override
	public final void onPercentCompleted(final double percentage, final long bytesProcessed, final long totalBytesToProcess) {
		SwingUtilities.invokeLater(() -> {
			final var params = new ArrayList<>();
			params.add(ByteConverter.convert(bytesProcessed));
			params.add((int) percentage);

			status.setStatusInfoMessage(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_BYTES_PROCESSED, params.toArray()));
		});
	}

	/**
	 * @param clientPath
	 */
	public void startDownload(final String clientPath) {
		status.setStatusInfoMessage(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_STATUS_PREPARE_DOWNLOAD));
		status.setBusy(true);

		logger.debug("Download file to '{}'", clientPath);

		new SwingWorker<Void, Void>() {
			/*
			 * (non-Javadoc)
			 * @see javax.swing.SwingWorker#doInBackground()
			 */
			@Override
			protected Void doInBackground() throws Exception {
				final String serverPath = prepareDownload();

				if (serverPath == null || serverPath.isEmpty())
					return null;

				// Get a file transport handler and download the file
				ServiceLocator.getFileTransportHandler().downloadFile(clientPath, serverPath);

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.SwingWorker#done()
			 */
			@Override
			protected void done() {
				status.setBusy(false);
				DataProcessedEventController.removeListener(AbstractFileDownloadOperation.this);

				try {
					// We have to call method get() in order to see if an exception occurred
					get();

					status.setStatusInfoMessage(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_FINISHED));
				}
				catch (final InterruptedException ex) {
					Thread.currentThread().interrupt();
					logger.error("Download operation has been interrupted!", ex);

					status.setStatusErrorMessage(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_ERROR) + ex.getMessage());
				}
				catch (final Exception e) {
					logger.error("Error while performing download operation!", e);

					status.setStatusErrorMessage(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_ERROR) + e.getMessage());
				}
			}
		}.execute();
	}

}
