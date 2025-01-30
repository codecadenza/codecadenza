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
package net.codecadenza.runtime.webclient.vaadin.util;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.router.BeforeEnterEvent;
import com.vaadin.flow.router.RouteParameters;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

/**
 * <p>
 * Utility class for the internal navigation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Navigator implements Serializable {
	public static final String ROUTE_PARAMETER_ID = "id";
	private static final String ERROR_MSG = "The path parameter \"" + ROUTE_PARAMETER_ID + "\" is missing!";
	private static final long serialVersionUID = 2236824105058259170L;

	private final Component component;

	/**
	 * Constructor
	 * @param component
	 */
	public Navigator(Component component) {
		this.component = component;
	}

	/**
	 * Navigate to the view of the given class
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass) {
		component.getUI().ifPresent(ui -> ui.navigate(viewClass));
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, String id) {
		component.getUI().ifPresent(ui -> {
			String encodedValue = id;

			try {
				encodedValue = URLEncoder.encode(id, StandardCharsets.UTF_8.toString());
			}
			catch (final UnsupportedEncodingException e) {
				// Ignored!
			}

			ui.navigate(viewClass, new RouteParameters(ROUTE_PARAMETER_ID, encodedValue));
		});
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, Long id) {
		navigateTo(viewClass, Long.toString(id));
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, Integer id) {
		navigateTo(viewClass, Integer.toString(id));
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, UUID id) {
		navigateTo(viewClass, id.toString());
	}

	/**
	 * Navigate back
	 */
	public void navigateBack() {
		component.getUI().ifPresent(ui -> ui.getPage().getHistory().back());
	}

	/**
	 * Get the string ID parameter from the provided route parameters
	 * @param event
	 * @return the ID
	 * @throws IllegalStateException if the parameter is missing
	 */
	public String getStringIdParameter(BeforeEnterEvent event) {
		final String id = event.getRouteParameters().get(ROUTE_PARAMETER_ID).orElseThrow(() -> new IllegalStateException(ERROR_MSG));

		return URLDecoder.decode(id, StandardCharsets.UTF_8);
	}

	/**
	 * Get the long ID parameter from the provided route parameters
	 * @param event
	 * @return the ID
	 * @throws IllegalStateException if the parameter is missing
	 * @throws NumberFormatException if the parameter could not be converted to a long
	 */
	public long getLongIdParameter(BeforeEnterEvent event) {
		return event.getRouteParameters().getLong(ROUTE_PARAMETER_ID).orElseThrow(() -> new IllegalStateException(ERROR_MSG));
	}

	/**
	 * Get the integer ID parameter from the provided route parameters
	 * @param event
	 * @return the ID
	 * @throws IllegalStateException if the parameter is missing
	 * @throws NumberFormatException if the parameter could not be converted to an integer
	 */
	public int getIntIdParameter(BeforeEnterEvent event) {
		return event.getRouteParameters().getInteger(ROUTE_PARAMETER_ID).orElseThrow(() -> new IllegalStateException(ERROR_MSG));
	}

	/**
	 * Get the {@link UUID} ID parameter from the provided route parameters
	 * @param event
	 * @return the ID
	 * @throws IllegalStateException if the parameter is missing
	 * @throws IllegalArgumentException if the parameter could not be converted to a {@link UUID}
	 */
	public UUID getUuidIdParameter(BeforeEnterEvent event) {
		final String id = event.getRouteParameters().get(ROUTE_PARAMETER_ID).orElseThrow(() -> new IllegalStateException(ERROR_MSG));

		return UUID.fromString(id);
	}

}
