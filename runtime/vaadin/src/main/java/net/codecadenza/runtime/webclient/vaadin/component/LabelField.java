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
package net.codecadenza.runtime.webclient.vaadin.component;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.HasValue.ValueChangeEvent;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.shared.Registration;

/**
 * <p>
 * Component for binding {@link String} values to a label
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Tag("div")
public class LabelField extends Component implements HasValue<ValueChangeEvent<String>, String>, HasSize {
	private static final long serialVersionUID = -3981148882836185669L;

	private final NativeLabel label = new NativeLabel();

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		getElement().appendChild(label.getElement());
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#getValue()
	 */
	@Override
	public String getValue() {
		return label.getText();
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setValue(java.lang.Object)
	 */
	@Override
	public void setValue(String value) {
		label.setText(value);
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#addValueChangeListener(com.vaadin.flow.component.HasValue.ValueChangeListener)
	 */
	@Override
	public Registration addValueChangeListener(ValueChangeListener<? super ValueChangeEvent<String>> valueChangeListener) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#isReadOnly()
	 */
	@Override
	public boolean isReadOnly() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setReadOnly(boolean)
	 */
	@Override
	public void setReadOnly(boolean readOnly) {
		// No implementation required as a label is read-only by default!
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#isRequiredIndicatorVisible()
	 */
	@Override
	public boolean isRequiredIndicatorVisible() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasValue#setRequiredIndicatorVisible(boolean)
	 */
	@Override
	public void setRequiredIndicatorVisible(boolean requiredIndicatorVisible) {
		// No implementation required as a label has no required indicator
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return label.getTitle().orElse(null);
	}

	/**
	 * @param title
	 */
	public void setTitle(String title) {
		label.setTitle(title);
	}

	/**
	 * @param enabled
	 */
	public void setEnabled(boolean enabled) {
		label.setEnabled(enabled);
	}

}
