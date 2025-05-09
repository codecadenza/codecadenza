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

import java.lang.ref.WeakReference;
import net.codecadenza.eclipse.diagram.domain.edit.parts.CodeCadenzaEditPartFactory;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartFactory;
import org.eclipse.gmf.runtime.common.core.service.IOperation;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.diagram.ui.services.editpart.AbstractEditPartProvider;
import org.eclipse.gmf.runtime.diagram.ui.services.editpart.CreateGraphicEditPartOperation;
import org.eclipse.gmf.runtime.diagram.ui.services.editpart.IEditPartOperation;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Edit part provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaEditPartProvider extends AbstractEditPartProvider {
	private EditPartFactory factory;
	private boolean allowCaching;
	private WeakReference<IGraphicalEditPart> cachedPart;
	private WeakReference<View> cachedView;

	/**
	 * Constructor
	 */
	public CodeCadenzaEditPartProvider() {
		setFactory(new CodeCadenzaEditPartFactory());
		setAllowCaching(true);
	}

	/**
	 * @return the edit part factory
	 */
	public final EditPartFactory getFactory() {
		return factory;
	}

	/**
	 * @param factory
	 */
	protected void setFactory(EditPartFactory factory) {
		this.factory = factory;
	}

	/**
	 * @return true if caching is allowed
	 */
	public final synchronized boolean isAllowCaching() {
		return allowCaching;
	}

	/**
	 * @param allowCaching
	 */
	protected synchronized void setAllowCaching(boolean allowCaching) {
		this.allowCaching = allowCaching;

		if (!allowCaching) {
			cachedPart = null;
			cachedView = null;
		}
	}

	/**
	 * @param view
	 * @return the graphical edit part
	 */
	protected IGraphicalEditPart createEditPart(View view) {
		final EditPart part = factory.createEditPart(null, view);

		if (part instanceof final IGraphicalEditPart graphicalEditPart)
			return graphicalEditPart;

		return null;
	}

	/**
	 * @param view
	 * @return the graphical edit part from the cache
	 */
	protected IGraphicalEditPart getCachedPart(View view) {
		if (cachedView != null && cachedView.get() == view)
			return cachedPart.get();

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.services.editpart.AbstractEditPartProvider#createGraphicEditPart(org.eclipse.gmf.
	 * runtime.notation.View)
	 */
	@Override
	public synchronized IGraphicalEditPart createGraphicEditPart(View view) {
		if (isAllowCaching()) {
			final IGraphicalEditPart part = getCachedPart(view);
			cachedPart = null;
			cachedView = null;

			if (part != null)
				return part;
		}

		return createEditPart(view);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.services.editpart.AbstractEditPartProvider#
	 * provides(org.eclipse.gmf.runtime.common.core.service.IOperation)
	 */
	@Override
	public synchronized boolean provides(IOperation operation) {
		if (operation instanceof CreateGraphicEditPartOperation) {
			final View view = ((IEditPartOperation) operation).getView();

			if (!DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(view)))
				return false;

			if (isAllowCaching() && getCachedPart(view) != null)
				return true;

			final IGraphicalEditPart part = createEditPart(view);

			if (part != null) {
				if (isAllowCaching()) {
					cachedPart = new WeakReference<>(part);
					cachedView = new WeakReference<>(view);
				}

				return true;
			}
		}

		return false;
	}

}
