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
package net.codecadenza.eclipse.shared.dialog;

import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.util.Collection;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Abstract input dialog that provides a proposal lookup feature
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the proposal input dialog
 */
public abstract class AbstractProposalInputDialog<T> extends Dialog {
	private final String title;
	private final String message;
	private T selectedItem;
	private AbstractProposalTextField<T> txtInput;
	private String inputText;

	/**
	 * Constructor
	 * @param parentShell
	 * @param title
	 * @param message
	 * @param selectedItem
	 */
	protected AbstractProposalInputDialog(Shell parentShell, String title, String message, T selectedItem) {
		super(parentShell);

		this.title = title;
		this.message = message;
		this.selectedItem = selectedItem;
	}

	/**
	 * @param filter
	 * @return the proposal data
	 */
	public abstract Collection<T> getProposalData(String filter);

	/**
	 * @param element
	 * @return the proposal label
	 */
	public abstract String getProposalLabel(T element);

	/**
	 * Callback method that is triggered as soon as an item is selected
	 * @param element
	 */
	@SuppressWarnings("unused")
	public void onSelection(T element) {

	}

	/**
	 * @return the selected item
	 */
	public T getSelectedItem() {
		return selectedItem;
	}

	/**
	 * @return the string that has been entered into the proposal text field
	 */
	public String getInputText() {
		return inputText;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			selectedItem = txtInput.getSelectedItem();
			inputText = txtInput.getControl().getText();
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);

		if (title != null)
			shell.setText(title);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		if (message != null) {
			final var gdMessage = new GridData(
					GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_CENTER);
			gdMessage.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);

			final var lblMessage = new Label(panDialogArea, SWT.WRAP);
			lblMessage.setText(message);
			lblMessage.setLayoutData(gdMessage);
		}

		txtInput = new AbstractProposalTextField<>(panDialogArea, SWT.SINGLE | SWT.BORDER, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<T> getProposalData(String filter) {
				return AbstractProposalInputDialog.this.getProposalData(filter);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang.Object)
			 */
			@Override
			public String getProposalLabel(T element) {
				return AbstractProposalInputDialog.this.getProposalLabel(element);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
			 */
			@Override
			public void onProposalAccepted(T element) {
				AbstractProposalInputDialog.this.onSelection(element);
			}
		};

		txtInput.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));
		txtInput.getControl().setFocus();
		txtInput.setSelectedItem(selectedItem);

		return panDialogArea;
	}

}
