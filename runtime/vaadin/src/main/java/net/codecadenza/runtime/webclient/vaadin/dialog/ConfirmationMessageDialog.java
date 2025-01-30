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
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_NO;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_YES;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import java.util.Locale;

/**
 * <p>
 * Confirmation message dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ConfirmationMessageDialog extends AbstractMessageDialog {
	private static final long serialVersionUID = -5881283567530455123L;

	/**
	 * Constructor
	 * @param title
	 * @param message
	 * @param locale
	 */
	public ConfirmationMessageDialog(String title, String message, Locale locale) {
		super(title, message, locale);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractMessageDialog#getStatusIcon()
	 */
	@Override
	protected Icon getStatusIcon() {
		return new Icon(VaadinIcon.QUESTION);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractMessageDialog#addButtons(com.vaadin.ui.HorizontalLayout)
	 */
	@Override
	protected void addButtons(HorizontalLayout buttonLayout) {
		final var cmdYes = new Button(i18n.getTranslation(CMD_YES));
		cmdYes.setId("cmdYes");
		cmdYes.addClickListener(event -> fireButtonClickEvent(ButtonType.YES));

		final var cmdNo = new Button(i18n.getTranslation(CMD_NO));
		cmdNo.setId("cmdNo");
		cmdNo.addClickListener(event -> fireButtonClickEvent(ButtonType.NO));

		final var cmdCancel = new Button(i18n.getTranslation(CMD_CANCEL));
		cmdCancel.setId("cmdCancel");
		cmdCancel.addClickListener(event -> fireButtonClickEvent(ButtonType.CANCEL));

		buttonLayout.add(cmdYes, cmdNo, cmdCancel);
	}

}
