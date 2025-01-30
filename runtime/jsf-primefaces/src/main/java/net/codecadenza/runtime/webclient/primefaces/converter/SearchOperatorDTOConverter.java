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
package net.codecadenza.runtime.webclient.primefaces.converter;

import jakarta.faces.component.UIComponent;
import jakarta.faces.context.FacesContext;
import jakarta.faces.convert.Converter;
import jakarta.faces.convert.FacesConverter;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.webclient.primefaces.search.JSFSearchOperatorHelper;

/**
 * <p>
 * Converter for search operator objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@FacesConverter("net.codecadenza.runtime.webclient.primefaces.converter.SearchOperatorDTOConverter")
public class SearchOperatorDTOConverter implements Converter<SearchOperatorDTO> {
	/*
	 * (non-Javadoc)
	 * @see jakarta.faces.convert.Converter#getAsObject(jakarta.faces.context.FacesContext, jakarta.faces.component.UIComponent,
	 * java.lang.String)
	 */
	@Override
	public SearchOperatorDTO getAsObject(FacesContext facesContext, UIComponent component, String submittedValue) {
		return JSFSearchOperatorHelper.getOperator(submittedValue);
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.faces.convert.Converter#getAsString(jakarta.faces.context.FacesContext, jakarta.faces.component.UIComponent,
	 * java.lang.Object)
	 */
	@Override
	public String getAsString(FacesContext facesContext, UIComponent component, SearchOperatorDTO value) {
		if (value == null)
			return null;

		return value.getValue();
	}

}
