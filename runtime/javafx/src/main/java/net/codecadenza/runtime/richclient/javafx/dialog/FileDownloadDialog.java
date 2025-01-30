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
package net.codecadenza.runtime.richclient.javafx.dialog;

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FILE_DOWNLOAD_DIALOG_BYTES_PROCESSED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FILE_DOWNLOAD_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;

import java.io.File;
import java.util.ArrayList;
import javafx.application.Platform;
import javafx.concurrent.Task;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.VBox;
import javafx.stage.StageStyle;
import javafx.stage.Window;
import net.codecadenza.runtime.file.ByteConverter;
import net.codecadenza.runtime.richclient.transport.ServiceLocator;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedEventController;
import net.codecadenza.runtime.richclient.transport.event.DataProcessedListener;

/**
 * <p>
 * File download dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileDownloadDialog extends AbstractBaseDialog implements DataProcessedListener {
	private final File targetFile;
	private final String serverFilePath;
	private ProgressBar progressBar;
	private Label lblMessage;
	private Task<Void> downloadTask;
	private Exception exception;

	/**
	 * Constructor
	 * @param owner
	 * @param serverFilePath
	 * @param targetFile
	 */
	public FileDownloadDialog(Window owner, String serverFilePath, File targetFile) {
		super(owner, getTranslation(FILE_DOWNLOAD_DIALOG_TITLE));

		this.targetFile = targetFile;
		this.serverFilePath = serverFilePath;

		setSize(300, 150);
		setResizable(false);

		initStyle(StageStyle.UTILITY);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOpen()
	 */
	@Override
	public void onOpen() {
		startDownload(serverFilePath, targetFile);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createButtons()
	 */
	@Override
	protected void createButtons() {
		addButton(DialogButtonType.CANCEL, getTranslation(CMD_CANCEL), false, true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onCancelPressed()
	 */
	@Override
	protected void onCancelPressed() {
		if (downloadTask != null)
			downloadTask.cancel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		final var panRoot = new VBox(10);

		panRoot.getChildren().add(progressBar = new ProgressBar(0));
		panRoot.getChildren().add(lblMessage = new Label());

		progressBar.setPrefWidth(getWidth() - 20);
		lblMessage.setPrefWidth(getWidth() - 20);

		return panRoot;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.transport.event.DataProcessedListener#onPercentCompleted(double, long, long)
	 */
	@Override
	public void onPercentCompleted(double percentage, long numberOfBytes, long totalNumberOfBytes) {
		Platform.runLater(() -> {
			progressBar.setProgress(percentage / 100);

			final var params = new ArrayList<>();
			params.add(ByteConverter.convert(numberOfBytes));
			params.add((int) percentage);

			lblMessage.setText(getTranslation(FILE_DOWNLOAD_DIALOG_BYTES_PROCESSED, params.toArray()));
		});
	}

	/**
	 * Start the download operation
	 * @param serverFilePath
	 * @param targetFile
	 */
	public void startDownload(final String serverFilePath, final File targetFile) {
		DataProcessedEventController.addListener(this);

		downloadTask = new Task<>() {
			/*
			 * (non-Javadoc)
			 * @see javafx.concurrent.Task#call()
			 */
			@Override
			protected Void call() throws Exception {
				// Download a file
				ServiceLocator.downloadFile(targetFile.getAbsolutePath(), serverFilePath);

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see java.util.concurrent.FutureTask#done()
			 */
			@Override
			protected void done() {
				DataProcessedEventController.removeListener(FileDownloadDialog.this);

				// Close this dialog as soon as the download has been finished!
				Platform.runLater(FileDownloadDialog.this::close);
			}

			/*
			 * (non-Javadoc)
			 * @see javafx.concurrent.Task#succeeded()
			 */
			@Override
			protected void succeeded() {
				returnCode = DialogButtonType.OK;
			}

			/*
			 * (non-Javadoc)
			 * @see javafx.concurrent.Task#failed()
			 */
			@Override
			protected void failed() {
				if (downloadTask.getException() != null)
					exception = new Exception(downloadTask.getException());
			}
		};

		new Thread(downloadTask).start();
	}

	/**
	 * @return an exception if something went wrong. It returns null if the download was finished successfully!
	 */
	public Exception getException() {
		return exception;
	}

}
