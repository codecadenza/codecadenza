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
package net.codecadenza.eclipse.diagram.domain.edit.parts;

import net.codecadenza.eclipse.diagram.domain.edit.policies.EnumLiteralItemSemanticEditPolicy;
import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaTextSelectionEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.StackLayout;
import org.eclipse.draw2d.ToolbarLayout;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPolicy;
import org.eclipse.gef.editpolicies.LayoutEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.ConstrainedToolbarLayoutEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.EditPolicyRoles;
import org.eclipse.gmf.runtime.draw2d.ui.figures.ConstrainedToolbarLayout;
import org.eclipse.gmf.runtime.draw2d.ui.figures.WrappingLabel;
import org.eclipse.gmf.runtime.gef.ui.figures.DefaultSizeNodeFigure;
import org.eclipse.gmf.runtime.gef.ui.figures.NodeFigure;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.graphics.Font;

/**
 * <p>
 * Enumeration literal edit part
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumLiteralEditPart extends ShapeNodeEditPart {
	public static final int VISUAL_ID = 3002;
	protected IFigure contentPane;
	protected IFigure primaryShape;

	/**
	 * @param view
	 */
	public EnumLiteralEditPart(View view) {
		super(view);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart#createDefaultEditPolicies()
	 */
	@Override
	protected void createDefaultEditPolicies() {
		super.createDefaultEditPolicies();

		installEditPolicy(EditPolicyRoles.SEMANTIC_ROLE, new EnumLiteralItemSemanticEditPolicy());
		installEditPolicy(EditPolicy.LAYOUT_ROLE, createLayoutEditPolicy());
	}

	/**
	 * @return the layout policy
	 */
	protected LayoutEditPolicy createLayoutEditPolicy() {
		return new ConstrainedToolbarLayoutEditPolicy() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.ConstrainedToolbarLayoutEditPolicy#createChildEditPolicy(org.
			 * eclipse.gef.EditPart)
			 */
			@Override
			protected EditPolicy createChildEditPolicy(EditPart child) {
				if (child.getEditPolicy(EditPolicy.PRIMARY_DRAG_ROLE) == null && child instanceof ITextAwareEditPart)
					return new CodeCadenzaTextSelectionEditPolicy();

				return super.createChildEditPolicy(child);
			}
		};
	}

	/**
	 * @return the node shape
	 */
	protected IFigure createNodeShape() {
		final var figure = new EnumerationLiteralFigure();

		return primaryShape = figure;
	}

	/**
	 * @return the primary shape
	 */
	public EnumerationLiteralFigure getPrimaryShape() {
		return (EnumerationLiteralFigure) primaryShape;
	}

	/**
	 * @param childEditPart
	 * @return the fixed child
	 */
	protected boolean addFixedChild(EditPart childEditPart) {
		if (childEditPart instanceof final EnumLiteralNameEditPart enumLiteralNameEditPart) {
			enumLiteralNameEditPart.setLabel(getPrimaryShape().getFigureEnumerationLiteralNameFigure());
			return true;
		}

		return false;
	}

	/**
	 * @param childEditPart
	 * @return always false
	 */
	@SuppressWarnings("unused")
	protected boolean removeFixedChild(EditPart childEditPart) {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editparts.AbstractGraphicalEditPart#addChildVisual(org.eclipse.gef.EditPart, int)
	 */
	@Override
	protected void addChildVisual(EditPart childEditPart, int index) {
		if (addFixedChild(childEditPart))
			return;

		super.addChildVisual(childEditPart, -1);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editparts.AbstractGraphicalEditPart#removeChildVisual(org.eclipse.gef.EditPart)
	 */
	@Override
	protected void removeChildVisual(EditPart childEditPart) {
		if (removeFixedChild(childEditPart))
			return;

		super.removeChildVisual(childEditPart);
	}

	/**
	 * @return the node figure
	 */
	protected NodeFigure createNodePlate() {
		return new DefaultSizeNodeFigure(getMapMode().DPtoLP(70), getMapMode().DPtoLP(20));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart#createNodeFigure()
	 */
	@Override
	protected NodeFigure createNodeFigure() {
		final NodeFigure figure = createNodePlate();
		figure.setLayoutManager(new StackLayout());

		final IFigure shape = createNodeShape();

		figure.add(shape);

		contentPane = setupContentPane(shape);

		return figure;
	}

	/**
	 * The default implementation treats the passed figure as the content pane
	 * @param nodeShape
	 * @return the figure
	 */
	protected IFigure setupContentPane(IFigure nodeShape) {
		if (nodeShape.getLayoutManager() == null) {
			final var layout = new ConstrainedToolbarLayout();
			layout.setSpacing(getMapMode().DPtoLP(5));

			nodeShape.setLayoutManager(layout);
		}

		return nodeShape;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editparts.AbstractGraphicalEditPart#getContentPane()
	 */
	@Override
	public IFigure getContentPane() {
		if (contentPane != null)
			return contentPane;

		return super.getContentPane();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.GraphicalEditPart#getPrimaryChildEditPart()
	 */
	@Override
	public EditPart getPrimaryChildEditPart() {
		return getChildBySemanticHint(CodeCadenzaVisualIDRegistry.getType(EnumLiteralNameEditPart.VISUAL_ID));
	}

	public class EnumerationLiteralFigure extends RectangleFigure {
		private static final Font FFIGUREENUMERATIONLITERALNAMEFIGURE_FONT = JFaceResources.getFont(JFaceResources.TEXT_FONT);

		private WrappingLabel fFigureEnumerationLiteralNameFigure;

		/**
		 * Constructor
		 */
		public EnumerationLiteralFigure() {
			final var layoutThis = new ToolbarLayout();
			layoutThis.setStretchMinorAxis(true);
			layoutThis.setMinorAlignment(ToolbarLayout.ALIGN_TOPLEFT);
			layoutThis.setSpacing(0);
			layoutThis.setHorizontal(false);

			this.setLayoutManager(layoutThis);
			this.setForegroundColor(ColorConstants.white);
			this.setBackgroundColor(ColorConstants.white);
			this.setMaximumSize(new Dimension(getMapMode().DPtoLP(1000), getMapMode().DPtoLP(20)));
			this.setMinimumSize(new Dimension(getMapMode().DPtoLP(20), getMapMode().DPtoLP(20)));

			createContents();
		}

		/**
		 * Create the contents
		 */
		private void createContents() {
			fFigureEnumerationLiteralNameFigure = new WrappingLabel();
			fFigureEnumerationLiteralNameFigure.setForegroundColor(ColorConstants.black);
			fFigureEnumerationLiteralNameFigure.setFont(FFIGUREENUMERATIONLITERALNAMEFIGURE_FONT);

			this.add(fFigureEnumerationLiteralNameFigure);
		}

		private boolean myUseLocalCoordinates;

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.draw2d.Figure#useLocalCoordinates()
		 */
		@Override
		protected boolean useLocalCoordinates() {
			return myUseLocalCoordinates;
		}

		/**
		 * @param useLocalCoordinates
		 */
		protected void setUseLocalCoordinates(boolean useLocalCoordinates) {
			myUseLocalCoordinates = useLocalCoordinates;
		}

		/**
		 * @return the wrapping label
		 */
		public WrappingLabel getFigureEnumerationLiteralNameFigure() {
			return fFigureEnumerationLiteralNameFigure;
		}
	}

}
