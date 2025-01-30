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

import com.vaadin.flow.component.Tag;
import com.vaadin.flow.data.provider.AbstractBackEndDataProvider;
import java.util.Locale;
import net.codecadenza.runtime.webclient.vaadin.provider.data.BackEndDataProvider;

/**
 * <p>
 * Component for selecting items from a list
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type that is used by this component
 */
@Tag("div")
public class DualListField<T> extends AbstractDualListField<T> {
	private static final long serialVersionUID = -3732740669740101456L;

	private BackEndDataProvider<T> dataProvider;

	/**
	 * Constructor
	 * @param locale
	 */
	public DualListField(Locale locale) {
		super(locale);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDualListField#getDataProvider()
	 */
	@Override
	public AbstractBackEndDataProvider<T, String> getDataProvider() {
		return dataProvider;
	}

	/**
	 * @param dataProvider
	 */
	public void setDataProvider(BackEndDataProvider<T> dataProvider) {
		this.dataProvider = dataProvider;
	}

}
