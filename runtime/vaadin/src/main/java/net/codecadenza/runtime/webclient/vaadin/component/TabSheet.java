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

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Composite;
import com.vaadin.flow.component.HasComponents;
import com.vaadin.flow.component.tabs.Tab;
import com.vaadin.flow.component.tabs.Tabs;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>
 * Component with multiple tabs that automatically shows the page content when a tab is clicked
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TabSheet extends Composite<Tabs> {
	private static final long serialVersionUID = -7452833543534905019L;

	private final HasComponents container;
	private final Tabs tabs = new Tabs();
	private final Map<Tab, Component> pageMap = new HashMap<>();
	private boolean firstPage = true;

	/**
	 * Constructor
	 * @param container
	 */
	public TabSheet(HasComponents container) {
		this.tabs.setWidthFull();

		this.tabs.addSelectedChangeListener(_ -> {
			// Hide all pages
			this.pageMap.values().forEach(page -> page.setVisible(false));

			// Show the selected page
			this.pageMap.get(this.tabs.getSelectedTab()).setVisible(true);
		});

		this.container = container;
		this.container.add(this.tabs);
	}

	/**
	 * Add a tab page
	 * @param page the component that represents the page
	 * @param title the page title
	 * @return the new tab page
	 */
	public Tab addTab(Component page, String title) {
		final var tab = new Tab(title);

		// Hide all pages but the first one!
		if (firstPage)
			firstPage = false;
		else
			page.setVisible(false);

		pageMap.put(tab, page);

		tabs.add(tab);

		container.add(page);

		return tab;
	}

}
