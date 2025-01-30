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
package net.codecadenza.eclipse.diagram.domain.providers;

import static net.codecadenza.eclipse.shared.Constants.IMG_DIAGRAM_SHORTCUT;

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.gef.EditPart;
import org.eclipse.gmf.runtime.common.core.service.AbstractProvider;
import org.eclipse.gmf.runtime.common.core.service.IOperation;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ConnectionEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.services.decorator.AbstractDecorator;
import org.eclipse.gmf.runtime.diagram.ui.services.decorator.CreateDecoratorsOperation;
import org.eclipse.gmf.runtime.diagram.ui.services.decorator.IDecoratorProvider;
import org.eclipse.gmf.runtime.diagram.ui.services.decorator.IDecoratorTarget;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Shortcut decoration provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaShortcutsDecoratorProvider extends AbstractProvider implements IDecoratorProvider {
	public static final String SHORTCUTS_DECORATOR_ID = "shortcuts";

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.common.core.service.IProvider#provides(org.eclipse.gmf.runtime.common.core.service.IOperation)
	 */
	@Override
	public boolean provides(IOperation operation) {
		if (!(operation instanceof final CreateDecoratorsOperation createDecoratorsOperation))
			return false;

		final IDecoratorTarget decoratorTarget = createDecoratorsOperation.getDecoratorTarget();
		final View view = decoratorTarget.getAdapter(View.class);

		return view != null && DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(view));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.services.decorator.IDecoratorProvider#
	 * createDecorators(org.eclipse.gmf.runtime.diagram.ui.services.decorator.IDecoratorTarget)
	 */
	@Override
	public void createDecorators(IDecoratorTarget decoratorTarget) {
		final View view = decoratorTarget.getAdapter(View.class);

		if (view != null) {
			final EAnnotation annotation = view.getEAnnotation("Shortcut");

			if (annotation != null)
				decoratorTarget.installDecorator(SHORTCUTS_DECORATOR_ID, new ShortcutsDecorator(decoratorTarget));
		}
	}

	protected class ShortcutsDecorator extends AbstractDecorator {
		/**
		 * @param decoratorTarget
		 */
		public ShortcutsDecorator(IDecoratorTarget decoratorTarget) {
			super(decoratorTarget);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gmf.runtime.diagram.ui.services.decorator.IDecorator#activate()
		 */
		@Override
		public void activate() {
			refresh();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gmf.runtime.diagram.ui.services.decorator.IDecorator#refresh()
		 */
		@Override
		public void refresh() {
			removeDecoration();

			final var editPart = (EditPart) getDecoratorTarget().getAdapter(EditPart.class);
			final Image image = CodeCadenzaResourcePlugin.getImage(IMG_DIAGRAM_SHORTCUT);

			if (editPart instanceof ShapeEditPart)
				setDecoration(getDecoratorTarget().addShapeDecoration(image, IDecoratorTarget.Direction.SOUTH_WEST, 0, false));
			else if (editPart instanceof ConnectionEditPart)
				setDecoration(getDecoratorTarget().addConnectionDecoration(image, 50, false));
		}
	}

}
