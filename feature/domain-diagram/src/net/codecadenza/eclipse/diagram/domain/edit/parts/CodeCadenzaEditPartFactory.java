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

import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import org.eclipse.draw2d.FigureUtilities;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.geometry.Dimension;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartFactory;
import org.eclipse.gef.tools.CellEditorLocator;
import org.eclipse.gmf.runtime.diagram.ui.editparts.ITextAwareEditPart;
import org.eclipse.gmf.runtime.draw2d.ui.figures.WrappingLabel;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Edit part factory
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaEditPartFactory implements EditPartFactory {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.EditPartFactory#createEditPart(org.eclipse.gef.EditPart, java.lang.Object)
	 */
	@Override
	public EditPart createEditPart(EditPart context, Object model) {
		if (model instanceof final View view) {
			switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
				case DomainNamespaceEditPart.VISUAL_ID:
					return new DomainNamespaceEditPart(view);

				case DomainObjectEditPart.VISUAL_ID:
					return new DomainObjectEditPart(view);

				case DomainObjectNameEditPart.VISUAL_ID:
					return new DomainObjectNameEditPart(view);

				case JavaEnumEditPart.VISUAL_ID:
					return new JavaEnumEditPart(view);

				case JavaEnumNameEditPart.VISUAL_ID:
					return new JavaEnumNameEditPart(view);

				case DomainAttributeEditPart.VISUAL_ID:
					return new DomainAttributeEditPart(view);

				case DomainAttributeNameEditPart.VISUAL_ID:
					return new DomainAttributeNameEditPart(view);

				case EnumLiteralEditPart.VISUAL_ID:
					return new EnumLiteralEditPart(view);

				case EnumLiteralNameEditPart.VISUAL_ID:
					return new EnumLiteralNameEditPart(view);

				case DomainAttributeCompartmentEditPart.VISUAL_ID:
					return new DomainAttributeCompartmentEditPart(view);

				case JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID:
					return new JavaEnumEnumerationLiteralCompartmentEditPart(view);

				case OneToOneAssociationEditPart.VISUAL_ID:
					return new OneToOneAssociationEditPart(view);

				case ManyToManyAssociationEditPart.VISUAL_ID:
					return new ManyToManyAssociationEditPart(view);

				case ManyToOneAssociationEditPart.VISUAL_ID:
					return new ManyToOneAssociationEditPart(view);

				case OneToManyAssociationEditPart.VISUAL_ID:
					return new OneToManyAssociationEditPart(view);

				case EnumAssociationEditPart.VISUAL_ID:
					return new EnumAssociationEditPart(view);

				case DomainObjectInheritanceEditPart.VISUAL_ID:
					return new DomainObjectInheritanceEditPart(view);
			}
		}

		return createUnrecognizedEditPart();
	}

	/**
	 * @return the edit part
	 */
	private EditPart createUnrecognizedEditPart() {
		// Handle the creation of unrecognized child node EditParts here
		return null;
	}

	/**
	 * @param source
	 * @return the cell editor locator
	 */
	public static CellEditorLocator getTextCellEditorLocator(ITextAwareEditPart source) {
		if (source.getFigure() instanceof final WrappingLabel wrappingLabel)
			return new TextCellEditorLocator(wrappingLabel);

		return new LabelCellEditorLocator((Label) source.getFigure());
	}

	private static class TextCellEditorLocator implements CellEditorLocator {
		private final WrappingLabel wrapLabel;

		/**
		 * @param wrapLabel
		 */
		public TextCellEditorLocator(WrappingLabel wrapLabel) {
			this.wrapLabel = wrapLabel;
		}

		/**
		 * @return the wrapping label
		 */
		public WrappingLabel getWrapLabel() {
			return wrapLabel;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gef.tools.CellEditorLocator#relocate(org.eclipse.jface.viewers.CellEditor)
		 */
		@Override
		public void relocate(CellEditor celleditor) {
			final var text = (Text) celleditor.getControl();
			final Rectangle rect = getWrapLabel().getTextBounds().getCopy();
			getWrapLabel().translateToAbsolute(rect);

			if (getWrapLabel().isTextWrapOn() && !getWrapLabel().getText().isEmpty())
				rect.setSize(new Dimension(text.computeSize(rect.width, SWT.DEFAULT)));
			else {
				final int avr = (int) FigureUtilities.getFontMetrics(text.getFont()).getAverageCharacterWidth();
				rect.setSize(new Dimension(text.computeSize(SWT.DEFAULT, SWT.DEFAULT)).expand(avr * 2, 0));
			}

			if (!rect.equals(new Rectangle(text.getBounds())))
				text.setBounds(rect.x, rect.y, rect.width, rect.height);
		}
	}

	private static class LabelCellEditorLocator implements CellEditorLocator {
		private final Label label;

		/**
		 * @param label
		 */
		public LabelCellEditorLocator(Label label) {
			this.label = label;
		}

		/**
		 * @return the label
		 */
		public Label getLabel() {
			return label;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gef.tools.CellEditorLocator#relocate(org.eclipse.jface.viewers.CellEditor)
		 */
		@Override
		public void relocate(CellEditor celleditor) {
			final var text = (Text) celleditor.getControl();
			final Rectangle rect = getLabel().getTextBounds().getCopy();
			final int avr = (int) FigureUtilities.getFontMetrics(text.getFont()).getAverageCharacterWidth();

			getLabel().translateToAbsolute(rect);
			rect.setSize(new Dimension(text.computeSize(SWT.DEFAULT, SWT.DEFAULT)).expand(avr * 2, 0));

			if (!rect.equals(new Rectangle(text.getBounds())))
				text.setBounds(rect.x, rect.y, rect.width, rect.height);
		}
	}

}
