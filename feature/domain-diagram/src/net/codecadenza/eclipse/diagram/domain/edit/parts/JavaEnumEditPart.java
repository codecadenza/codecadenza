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

import net.codecadenza.eclipse.diagram.domain.edit.policies.DoubleClickEditPolicy;
import net.codecadenza.eclipse.diagram.domain.edit.policies.CodeCadenzaTextSelectionEditPolicy;
import net.codecadenza.eclipse.diagram.domain.edit.policies.JavaEnumItemSemanticEditPolicy;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.MarginBorder;
import org.eclipse.draw2d.PositionConstants;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.StackLayout;
import org.eclipse.draw2d.ToolbarLayout;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPolicy;
import org.eclipse.gef.Request;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.editpolicies.LayoutEditPolicy;
import org.eclipse.gmf.runtime.diagram.core.edithelpers.CreateElementRequestAdapter;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.ConstrainedToolbarLayoutEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.CreationEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.EditPolicyRoles;
import org.eclipse.gmf.runtime.diagram.ui.requests.CreateViewAndElementRequest;
import org.eclipse.gmf.runtime.draw2d.ui.figures.ConstrainedToolbarLayout;
import org.eclipse.gmf.runtime.draw2d.ui.figures.WrappingLabel;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.gmf.runtime.gef.ui.figures.DefaultSizeNodeFigure;
import org.eclipse.gmf.runtime.gef.ui.figures.NodeFigure;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.graphics.Font;

/**
 * <p>
 * Enumeration edit part
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaEnumEditPart extends ShapeNodeEditPart {
	public static final int VISUAL_ID = 2002;
	protected IFigure contentPane;
	protected IFigure primaryShape;

	/**
	 * @param view
	 */
	public JavaEnumEditPart(View view) {
		super(view);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart#createDefaultEditPolicies()
	 */
	@Override
	protected void createDefaultEditPolicies() {
		// Policy for opening elements by performing a double-click
		installEditPolicy(EditPolicyRoles.OPEN_ROLE, new DoubleClickEditPolicy());

		installEditPolicy(EditPolicyRoles.CREATION_ROLE, new CreationEditPolicy() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CreationEditPolicy#getCommand(org.eclipse.gef.Request)
			 */
			@Override
			public Command getCommand(Request request) {
				if (understandsRequest(request)) {
					if (request instanceof final CreateViewAndElementRequest createViewAndElementRequest) {
						final CreateElementRequestAdapter adapter = createViewAndElementRequest.getViewAndElementDescriptor()
								.getCreateElementRequestAdapter();
						final var type = (IElementType) adapter.getAdapter(IElementType.class);

						if (type == CodeCadenzaElementTypes.EnumLiteral_3002) {
							final EditPart compartmentEditPart = getChildBySemanticHint(
									CodeCadenzaVisualIDRegistry.getType(JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID));
							return compartmentEditPart == null ? null : compartmentEditPart.getCommand(request);
						}
					}

					return super.getCommand(request);
				}

				return null;
			}
		});

		super.createDefaultEditPolicies();

		installEditPolicy(EditPolicyRoles.SEMANTIC_ROLE, new JavaEnumItemSemanticEditPolicy());
		installEditPolicy(EditPolicy.LAYOUT_ROLE, createLayoutEditPolicy());
	}

	/**
	 * @return the layout edit policy
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
	 * @return the node shape figure
	 */
	protected IFigure createNodeShape() {
		final var figure = new EnumerationFigure();
		return primaryShape = figure;
	}

	/**
	 * @return the primary shape
	 */
	public EnumerationFigure getPrimaryShape() {
		return (EnumerationFigure) primaryShape;
	}

	/**
	 * @param childEditPart
	 * @return true if the operation was successful
	 */
	protected boolean addFixedChild(EditPart childEditPart) {
		if (childEditPart instanceof final JavaEnumNameEditPart enumNameEditPart) {
			enumNameEditPart.setLabel(getPrimaryShape().getFigureEnumerationNameFigure());
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
		return new DefaultSizeNodeFigure(getMapMode().DPtoLP(40), getMapMode().DPtoLP(40));
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
	 * @param nodeShape
	 * @return the figure
	 */
	protected IFigure setupContentPane(IFigure nodeShape) {
		if (nodeShape.getLayoutManager() == null) {
			final var layout = new ConstrainedToolbarLayout();
			layout.setSpacing(getMapMode().DPtoLP(5));

			nodeShape.setLayoutManager(layout);
		}

		return nodeShape; // use nodeShape itself as contentPane
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
		return getChildBySemanticHint(CodeCadenzaVisualIDRegistry.getType(JavaEnumNameEditPart.VISUAL_ID));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editparts.ShapeNodeEditPart#handleNotificationEvent(org.eclipse.emf.common.notify.
	 * Notification)
	 */
	@Override
	protected void handleNotificationEvent(Notification event) {
		if (event.getNotifier() == getModel() && EcorePackage.eINSTANCE.getEModelElement_EAnnotations().equals(event.getFeature()))
			handleMajorSemanticChange();
		else
			super.handleNotificationEvent(event);
	}

	public class EnumerationFigure extends RectangleFigure {
		private static final Font FFIGUREENUMERATIONNAMEFIGURE_FONT = JFaceResources.getFont(JFaceResources.TEXT_FONT);

		private WrappingLabel fFigureEnumerationNameFigure;

		/**
		 * Constructor
		 */
		public EnumerationFigure() {
			final var layoutThis = new ToolbarLayout();
			layoutThis.setStretchMinorAxis(true);
			layoutThis.setMinorAlignment(ToolbarLayout.ALIGN_CENTER);
			layoutThis.setSpacing(0);
			layoutThis.setHorizontal(false);

			this.setLayoutManager(layoutThis);
			this.setBackgroundColor(ColorConstants.white);

			createContents();
		}

		/**
		 * Create the contents
		 */
		private void createContents() {
			final var rect = new RectangleFigure() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.draw2d.RectangleFigure#fillShape(org.eclipse.draw2d.Graphics)
				 */
				@Override
				protected void fillShape(Graphics graphics) {
					graphics.setBackgroundColor(ColorConstants.white);
					graphics.setForegroundColor(ColorConstants.lightBlue);
					graphics.fillGradient(getBounds(), true);
				}
			};

			rect.setMaximumSize(new Dimension(getMapMode().DPtoLP(3000), getMapMode().DPtoLP(30)));
			rect.setMinimumSize(new Dimension(getMapMode().DPtoLP(30), getMapMode().DPtoLP(30)));
			rect.setBorder(
					new MarginBorder(getMapMode().DPtoLP(5), getMapMode().DPtoLP(5), getMapMode().DPtoLP(5), getMapMode().DPtoLP(5)));

			this.add(rect);

			final var layoutRect = new ToolbarLayout();
			layoutRect.setStretchMinorAxis(false);
			layoutRect.setMinorAlignment(ToolbarLayout.ALIGN_CENTER);
			layoutRect.setSpacing(0);
			layoutRect.setHorizontal(false);

			rect.setLayoutManager(layoutRect);

			fFigureEnumerationNameFigure = new WrappingLabel();
			fFigureEnumerationNameFigure.setForegroundColor(ColorConstants.black);
			fFigureEnumerationNameFigure.setAlignment(PositionConstants.CENTER);
			fFigureEnumerationNameFigure.setFont(FFIGUREENUMERATIONNAMEFIGURE_FONT);

			rect.add(fFigureEnumerationNameFigure);
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
		public WrappingLabel getFigureEnumerationNameFigure() {
			return fFigureEnumerationNameFigure;
		}
	}

}
