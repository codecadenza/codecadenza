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
package net.codecadenza.runtime.webclient.vaadin.dialog;

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_CANCEL;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_OK;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.INPUT_DIALOG_MSG_ERROR_NO_VALUE;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextField;
import java.util.Locale;

/**
 * <p>
 * Dialog that expects user input
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class InputDialog extends AbstractTitleDialog {
	private static final long serialVersionUID = -1771800075328301920L;

	private final TextField txtInput = new TextField();
	private final String inputPrompt;

	/**
	 * Constructor
	 * @param title
	 * @param inputPrompt
	 * @param locale
	 */
	public InputDialog(String title, String inputPrompt, Locale locale) {
		super(title, locale);

		this.inputPrompt = inputPrompt;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		super.onAttach(attachEvent);

		setModal(true);
		setCloseOnOutsideClick(false);
		setWidth(450, Unit.PIXELS);
		setHeight(200, Unit.PIXELS);

		txtInput.setPlaceholder(inputPrompt);
		txtInput.setWidthFull();
		txtInput.focus();

		add(txtInput);
	}

	/**
	 * @return the value
	 */
	public String getValue() {
		return txtInput.getValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog#addButtons(com.vaadin.flow.component.
	 * orderedlayout.HorizontalLayout)
	 */
	@Override
	protected void addButtons(HorizontalLayout hlButtons) {
		final var cmdOK = new Button(i18n.getTranslation(CMD_OK));

		cmdOK.addClickListener(event -> {
			final String value = txtInput.getValue();

			if (value == null || value.isEmpty()) {
				txtInput.setPlaceholder(i18n.getTranslation(INPUT_DIALOG_MSG_ERROR_NO_VALUE));
				return;
			}

			fireButtonClickEvent(ButtonType.OK);
		});

		final var cmdCancel = new Button(i18n.getTranslation(CMD_CANCEL));
		cmdCancel.addClickListener(event -> fireButtonClickEvent(ButtonType.CANCEL));

		hlButtons.add(cmdOK, cmdCancel);
	}

}
