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
import com.vaadin.flow.component.HasComponents;
import com.vaadin.flow.component.HasSize;
import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.dom.Element;

/**
 * <p>
 * This class represents a container component with a border and a title
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Tag("fieldset")
public class BorderPanel extends Component implements HasComponents, HasSize {
	private static final long serialVersionUID = 9055765949830119009L;

	private final Element legend = new Element("legend");
	private final VerticalLayout vlContent = new VerticalLayout();
	private final String title;

	/**
	 * Constructor
	 * @param title the title that should be displayed. If the provided value is either null or empty, only the border will be
	 *          displayed!
	 */
	public BorderPanel(String title) {
		this.title = title;
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		if (title != null && !title.isEmpty()) {
			legend.setText(title);

			getElement().appendChild(legend);
		}

		getElement().appendChild(vlContent.getElement());
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.HasComponents#add(com.vaadin.flow.component.Component[])
	 */
	@Override
	public void add(Component... components) {
		vlContent.add(components);
	}

}
