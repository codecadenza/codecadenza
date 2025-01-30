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
package net.codecadenza.eclipse.diagram.domain.edit.policies;

import java.util.Collections;
import java.util.List;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.Figure;
import org.eclipse.draw2d.Graphics;
import org.eclipse.draw2d.IFigure;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.RectangleFigure;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.gef.handles.MoveHandle;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.NonResizableEditPolicyEx;
import org.eclipse.gmf.runtime.diagram.ui.tools.DragEditPartsTrackerEx;
import org.eclipse.gmf.runtime.draw2d.ui.figures.WrappingLabel;

/**
 * <p>
 * Text edit policy
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaTextNonResizableEditPolicy extends NonResizableEditPolicyEx {
	private IFigure selectionFeedbackFigure;
	private IFigure focusFeedbackFigure;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editpolicies.SelectionEditPolicy#showPrimarySelection()
	 */
	@Override
	protected void showPrimarySelection() {
		if (getHostFigure() instanceof final WrappingLabel wrappingLabel) {
			wrappingLabel.setSelected(true);
			wrappingLabel.setFocus(true);
		}
		else {
			showSelection();
			showFocus();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editpolicies.SelectionHandlesEditPolicy#showSelection()
	 */
	@Override
	protected void showSelection() {
		if (getHostFigure() instanceof final WrappingLabel wrappingLabel) {
			wrappingLabel.setSelected(true);
			wrappingLabel.setFocus(false);
		}
		else {
			hideSelection();
			addFeedback(selectionFeedbackFigure = createSelectionFeedbackFigure());
			refreshSelectionFeedback();
			hideFocus();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editpolicies.SelectionHandlesEditPolicy#hideSelection()
	 */
	@Override
	protected void hideSelection() {
		if (getHostFigure() instanceof final WrappingLabel wrappingLabel) {
			wrappingLabel.setSelected(false);
			wrappingLabel.setFocus(false);
		}
		else {
			if (selectionFeedbackFigure != null) {
				removeFeedback(selectionFeedbackFigure);
				selectionFeedbackFigure = null;
			}

			hideFocus();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editpolicies.NonResizableEditPolicy#showFocus()
	 */
	@Override
	protected void showFocus() {
		if (getHostFigure() instanceof final WrappingLabel wrappingLabel)
			wrappingLabel.setFocus(true);
		else {
			hideFocus();
			addFeedback(focusFeedbackFigure = createFocusFeedbackFigure());
			refreshFocusFeedback();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editpolicies.NonResizableEditPolicy#hideFocus()
	 */
	@Override
	protected void hideFocus() {
		if (getHostFigure() instanceof final WrappingLabel wrappingLabel)
			wrappingLabel.setFocus(false);
		else if (focusFeedbackFigure != null) {
			removeFeedback(focusFeedbackFigure);
			focusFeedbackFigure = null;
		}
	}

	/**
	 * @return the feedback bounds
	 */
	protected Rectangle getFeedbackBounds() {
		Rectangle bounds;

		if (getHostFigure() instanceof final Label label) {
			bounds = label.getTextBounds();
			bounds.intersect(getHostFigure().getBounds());
		}
		else
			bounds = getHostFigure().getBounds().getCopy();

		getHostFigure().getParent().translateToAbsolute(bounds);
		getFeedbackLayer().translateToRelative(bounds);

		return bounds;
	}

	/**
	 * @return the figure
	 */
	protected IFigure createSelectionFeedbackFigure() {
		if (getHostFigure() instanceof Label) {
			final var feedbackFigure = new Label();
			feedbackFigure.setOpaque(true);
			feedbackFigure.setBackgroundColor(ColorConstants.menuBackgroundSelected);
			feedbackFigure.setForegroundColor(ColorConstants.menuForegroundSelected);

			return feedbackFigure;
		}

		final var feedbackFigure = new RectangleFigure();
		feedbackFigure.setFill(false);

		return feedbackFigure;
	}

	/**
	 * @return the focus feedback figure
	 */
	protected IFigure createFocusFeedbackFigure() {
		return new Figure() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.draw2d.Figure#paintFigure(org.eclipse.draw2d.Graphics)
			 */
			@Override
			protected void paintFigure(Graphics graphics) {
				graphics.drawFocus(getBounds().getResized(-1, -1));
			}
		};
	}

	/**
	 * @param target
	 */
	protected void updateLabel(Label target) {
		final var source = (Label) getHostFigure();
		target.setText(source.getText());
		target.setTextAlignment(source.getTextAlignment());
		target.setFont(source.getFont());
	}

	/**
	 * Refresh selection feedback
	 */
	protected void refreshSelectionFeedback() {
		if (selectionFeedbackFigure != null) {
			if (selectionFeedbackFigure instanceof final Label label) {
				updateLabel(label);
				selectionFeedbackFigure.setBounds(getFeedbackBounds());
			}
			else
				selectionFeedbackFigure.setBounds(getFeedbackBounds().expand(5, 5));
		}
	}

	/**
	 * Refresh focus feedback
	 */
	protected void refreshFocusFeedback() {
		if (focusFeedbackFigure != null)
			focusFeedbackFigure.setBounds(getFeedbackBounds());
	}

	/**
	 * Refresh feedback
	 */
	public void refreshFeedback() {
		refreshSelectionFeedback();
		refreshFocusFeedback();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.editpolicies.NonResizableEditPolicy#createSelectionHandles()
	 */
	@Override
	protected List<MoveHandle> createSelectionHandles() {
		final var moveHandle = new MoveHandle(getHost());
		moveHandle.setBorder(null);
		moveHandle.setDragTracker(new DragEditPartsTrackerEx(getHost()));

		return Collections.singletonList(moveHandle);
	}

}
