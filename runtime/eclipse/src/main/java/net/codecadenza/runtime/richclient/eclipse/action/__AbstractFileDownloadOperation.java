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
package net.codecadenza.runtime.richclient.eclipse.action;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_ERROR;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_FILE_NOT_FOUND;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_TITLE_DOWNLOAD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_FILE_DOWNLOAD_OPERATION_STATUS_DOWNLOAD_PERFORM;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_FILE_DOWNLOAD_OPERATION_STATUS_PREPARE_DOWNLOAD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.InvocationTargetException;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedListener;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Display;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for download operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class __AbstractFileDownloadOperation implements IRunnableWithProgress, DataProcessedListener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private String clientPath;
	private IProgressMonitor monitor;

	/**
	 * Prepare the download
	 * @return the fully qualified path name of the file to download
	 * @throws Exception if the preparation of the download has failed
	 */
	public abstract String prepareDownload() throws Exception;

	/**
	 * An implementation must define how a file should be downloaded
	 * @param clientPath
	 * @param serverPath
	 * @throws Exception if the download operation has failed
	 */
	protected abstract void downloadFile(String clientPath, String serverPath) throws Exception;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.transport.event.DataProcessedListener#onPercentCompleted(double, long, long)
	 */
	@Override
	public final void onPercentCompleted(double percentage, long bytesProcessed, long totalBytesToProcess) {
		monitor.worked(1);
	}

	/**
	 * Callback method that will be called as soon as download has been finished
	 * @param path
	 * @throws Exception if an internal error has occurred
	 */
	@SuppressWarnings("unused")
	public void onDownloadFinished(String path) throws Exception {

	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public final void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
		DataProcessedEventController.addListener(this);

		this.monitor = monitor;

		try {
			// Prepare the download
			monitor.beginTask(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_STATUS_PREPARE_DOWNLOAD), IProgressMonitor.UNKNOWN);
			final String serverPath = prepareDownload();

			logger.debug("Download file '{}'", serverPath);

			if (serverPath == null || serverPath.isEmpty()) {
				monitor.setCanceled(true);

				final String title = getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_TITLE_DOWNLOAD);
				final String messageText = getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_FILE_NOT_FOUND);

				Display.getDefault()
						.syncExec(() -> MessageDialog.openInformation(Display.getCurrent().getActiveShell(), title, messageText));

				return;
			}

			monitor.beginTask(getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_STATUS_DOWNLOAD_PERFORM), 100);

			// Perform the download
			downloadFile(clientPath, serverPath);

			monitor.done();

			logger.debug("Download finished");

			onDownloadFinished(clientPath);
		}
		catch (final Exception e) {
			logger.error("Error while performing download operation!", e);

			final String title = getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_TITLE_DOWNLOAD);
			final String messageText = getTranslation(ABSTRACT_FILE_DOWNLOAD_OPERATION_MSG_DOWNLOAD_ERROR) + e.getMessage();

			Display.getDefault()
					.syncExec(() -> MessageDialog.openInformation(Display.getCurrent().getActiveShell(), title, messageText));
		}
	}

	/**
	 * @param clientPath
	 */
	public void setClientPath(String clientPath) {
		this.clientPath = clientPath;
	}

}
