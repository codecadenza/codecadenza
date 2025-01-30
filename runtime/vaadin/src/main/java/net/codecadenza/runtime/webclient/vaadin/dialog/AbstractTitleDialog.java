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

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_OK;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;

/**
 * <p>
 * Base class for all dialogs that need a title bar
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTitleDialog extends Dialog {
	public static final String DEFAULT_ICON_SIZE = "32px";
	private static final long serialVersionUID = -6624572154912842951L;

	private final Div divContent = new Div();
	private final String title;
	protected transient ButtonClickListener listener;
	protected InternalI18NService i18n;

	/**
	 * Callback listener for button click events
	 */
	public interface ButtonClickListener {
		/**
		 * Callback method that will be fired as soon as a button has been clicked
		 * @param type the type of the button that has been clicked
		 */
		void onButtonClick(ButtonType type);
	}

	/**
	 * Constructor
	 * @param title
	 * @param locale
	 */
	protected AbstractTitleDialog(String title, Locale locale) {
		this.i18n = new InternalI18NService(locale);
		this.title = title;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.dialog.Dialog#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		getElement().setAttribute("theme", "dialog-with-title-bar");

		final var closeIcon = new Icon(VaadinIcon.CLOSE);
		closeIcon.addClickListener(event -> close());

		final var divTitleBar = new Div();
		divTitleBar.addClassNames("titlebar-for-dialog", "draggable");
		divTitleBar.add(new Span(title), closeIcon);

		divContent.addClassNames("content-for-dialog");

		final var divButtonBar = new Div();
		divButtonBar.addClassNames("buttonbar-for-dialog");

		final var hlButtons = new HorizontalLayout();
		hlButtons.setPadding(true);

		addButtons(hlButtons);

		super.add(divTitleBar, divContent, divButtonBar, hlButtons);
	}

	/**
	 * @param hlButtons
	 */
	protected void addButtons(HorizontalLayout hlButtons) {
		final var cmdOK = new Button(i18n.getTranslation(CMD_OK));
		cmdOK.setId("cmdOK");
		cmdOK.addClickListener(event -> fireButtonClickEvent(ButtonType.OK));

		hlButtons.add(cmdOK);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.dialog.Dialog#add(com.vaadin.flow.component.Component[])
	 */
	@Override
	public void add(Component... components) {
		divContent.add(components);
	}

	/**
	 * @return the component where the specific dialog content should be added to
	 */
	public Div getContentArea() {
		return divContent;
	}

	/**
	 * @param type
	 */
	protected void fireButtonClickEvent(ButtonType type) {
		close();

		if (listener != null)
			listener.onButtonClick(type);
	}

	/**
	 * @param listener
	 */
	public void setButtonClickListener(ButtonClickListener listener) {
		this.listener = listener;
	}

}
