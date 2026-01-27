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

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.ModalityMode;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import java.util.Locale;

/**
 * <p>
 * Abstract base class for message dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractMessageDialog extends AbstractTitleDialog {
	private static final long serialVersionUID = -5386038401508161149L;

	private final String message;

	/**
	 * Constructor
	 * @param title
	 * @param message
	 * @param locale
	 */
	protected AbstractMessageDialog(String title, String message, Locale locale) {
		super(title, locale);

		this.message = message;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.dialog.Dialog#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		super.onAttach(attachEvent);

		setModality(ModalityMode.STRICT);
		setCloseOnOutsideClick(false);
		setWidth(380, Unit.PIXELS);
		setHeight(180, Unit.PIXELS);

		final var lblMessage = new NativeLabel(message);
		lblMessage.setId("lblMessage");

		final var iconStatus = getStatusIcon();
		iconStatus.getStyle().set("width", DEFAULT_ICON_SIZE);
		iconStatus.getStyle().set("height", DEFAULT_ICON_SIZE);
		iconStatus.setId("imgStatus");

		final var hlHeader = new HorizontalLayout();
		hlHeader.add(iconStatus, lblMessage);
		hlHeader.setWidthFull();
		hlHeader.setHeight(50, Unit.PIXELS);

		add(hlHeader);
	}

	/**
	 * An implementation class must provide a status icon!
	 * @return the status icon
	 */
	protected abstract Icon getStatusIcon();

}
