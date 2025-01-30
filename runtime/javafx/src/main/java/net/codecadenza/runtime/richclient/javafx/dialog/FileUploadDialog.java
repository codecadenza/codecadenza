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
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FILE_UPLOAD_DIALOG_BYTES_PROCESSED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.FILE_UPLOAD_DIALOG_TITLE;
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
 * File upload dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileUploadDialog extends AbstractBaseDialog implements DataProcessedListener {
	private final File uploadFile;
	private String path;
	private ProgressBar progressBar;
	private Label lblMessage;
	private Task<Void> uploadTask;
	private Throwable exception;

	/**
	 * Constructor
	 * @param owner
	 * @param uploadFile
	 */
	public FileUploadDialog(Window owner, File uploadFile) {
		super(owner, getTranslation(FILE_UPLOAD_DIALOG_TITLE));

		this.uploadFile = uploadFile;

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
		startUpload(uploadFile);
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
		if (uploadTask != null)
			uploadTask.cancel();
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

			lblMessage.setText(getTranslation(FILE_UPLOAD_DIALOG_BYTES_PROCESSED, params.toArray()));
		});
	}

	/**
	 * Start the upload operation
	 * @param uploadFile
	 */
	private void startUpload(final File uploadFile) {
		DataProcessedEventController.addListener(this);

		uploadTask = new Task<>() {
			/*
			 * (non-Javadoc)
			 * @see javafx.concurrent.Task#call()
			 */
			@Override
			protected Void call() throws Exception {
				// Upload the file
				path = ServiceLocator.uploadFile(uploadFile);

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see java.util.concurrent.FutureTask#done()
			 */
			@Override
			protected void done() {
				DataProcessedEventController.removeListener(FileUploadDialog.this);

				// Close this dialog as soon as the upload has been finished!
				Platform.runLater(FileUploadDialog.this::close);
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
				exception = uploadTask.getException();
			}
		};

		new Thread(uploadTask).start();
	}

	/**
	 * @return the fully qualified path of the uploaded file on the server
	 */
	public String getPath() {
		return path;
	}

	/**
	 * @return an exception if something went wrong. It returns null if the upload was finished successfully!
	 */
	public Throwable getException() {
		return exception;
	}

}
