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

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ERROR_MESSAGE_DIALOG_OUTPUT_CAUSE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ERROR_MESSAGE_DIALOG_OUTPUT_MESSAGE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.ERROR_MESSAGE_DIALOG_OUTPUT_STACK_TRACE;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import java.util.Locale;

/**
 * <p>
 * Error message dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ErrorMessageDialog extends AbstractTitleDialog {
	private static final long serialVersionUID = -6951610259316454378L;

	private final String message;
	private final Throwable throwable;

	/**
	 * Constructor
	 * @param title
	 * @param message
	 * @param throwable
	 * @param locale
	 */
	public ErrorMessageDialog(String title, String message, Throwable throwable, Locale locale) {
		super(title, locale);

		this.message = message;
		this.throwable = throwable;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.dialog.AbstractTitleDialog#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		super.onAttach(attachEvent);

		setModal(true);
		setResizable(true);
		setCloseOnOutsideClick(false);
		setWidth(450, Unit.PIXELS);
		setHeight(320, Unit.PIXELS);

		final var lblMessage = new NativeLabel(message);
		lblMessage.setId("lblMessage");

		final var iconStatus = new Icon(VaadinIcon.EXCLAMATION_CIRCLE);
		iconStatus.getStyle().set("width", DEFAULT_ICON_SIZE);
		iconStatus.getStyle().set("height", DEFAULT_ICON_SIZE);
		iconStatus.setId("imgStatus");

		final var hlHeader = new HorizontalLayout();
		hlHeader.add(iconStatus, lblMessage);
		hlHeader.setWidthFull();
		hlHeader.setHeight(50, Unit.PIXELS);

		final var txtStackTrace = new TextArea();
		txtStackTrace.setSizeFull();
		txtStackTrace.setId("txtStackTrace");
		txtStackTrace.setValue(generateStackTrace(throwable));
		txtStackTrace.setReadOnly(true);

		add(hlHeader, txtStackTrace);
	}

	/**
	 * Create a human-readable representation of the provided exception's stack trace
	 * @param throwable
	 * @return the generated string representation of the stack trace
	 */
	private String generateStackTrace(final Throwable throwable) {
		final var sb = new StringBuilder();
		sb.append(i18n.getTranslation(ERROR_MESSAGE_DIALOG_OUTPUT_MESSAGE));
		sb.append(": " + throwable.getMessage()).append("\n\n");
		sb.append(i18n.getTranslation(ERROR_MESSAGE_DIALOG_OUTPUT_STACK_TRACE) + ":\n");

		final StackTraceElement[] stackTrace = throwable.getStackTrace();

		for (final StackTraceElement element : stackTrace)
			sb.append(element.toString()).append("\n");

		final Throwable next = throwable.getCause();

		sb.append("\n");

		if (next != null) {
			sb.append(i18n.getTranslation(ERROR_MESSAGE_DIALOG_OUTPUT_CAUSE) + ": " + next + "\n");
			sb.append(generateStackTrace(next));
		}

		return sb.toString();
	}

}
