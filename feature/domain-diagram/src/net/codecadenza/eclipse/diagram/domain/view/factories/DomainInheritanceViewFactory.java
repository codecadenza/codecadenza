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
package net.codecadenza.eclipse.diagram.domain.view.factories;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectInheritanceEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.gmf.runtime.diagram.ui.view.factories.ConnectionViewFactory;
import org.eclipse.gmf.runtime.notation.NotationFactory;
import org.eclipse.gmf.runtime.notation.Style;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * View factory for domain object inheritance views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainInheritanceViewFactory extends ConnectionViewFactory {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.view.factories.ConnectionViewFactory#createStyles(org.eclipse.gmf.runtime.notation.
	 * View)
	 */
	@Override
	protected List<Style> createStyles(View view) {
		final var styles = new ArrayList<Style>();
		styles.add(NotationFactory.eINSTANCE.createConnectorStyle());
		styles.add(NotationFactory.eINSTANCE.createFontStyle());

		return styles;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.view.factories.ConnectionViewFactory#decorateView(org.eclipse.gmf.runtime.notation.
	 * View, org.eclipse.gmf.runtime.notation.View, org.eclipse.core.runtime.IAdaptable, java.lang.String, int, boolean)
	 */
	@Override
	protected void decorateView(View containerView, View view, IAdaptable semanticAdapter, String semanticHint, int index,
			boolean persisted) {
		if (semanticHint == null) {
			semanticHint = CodeCadenzaVisualIDRegistry.getType(DomainObjectInheritanceEditPart.VISUAL_ID);
			view.setType(semanticHint);
		}

		super.decorateView(containerView, view, semanticAdapter, semanticHint, index, persisted);
	}

}
