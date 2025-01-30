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
package net.codecadenza.eclipse.diagram.domain.edit.helpers;

import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import org.eclipse.draw2d.ColorConstants;
import org.eclipse.draw2d.ConnectionEndpointLocator;
import org.eclipse.draw2d.Label;
import org.eclipse.draw2d.PolygonDecoration;
import org.eclipse.draw2d.PolylineConnection;
import org.eclipse.draw2d.PolylineDecoration;
import org.eclipse.draw2d.RotatableDecoration;
import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.jface.resource.JFaceResources;

/**
 * <p>
 * Utility class for creating domain association decorations and labels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAssociationDecorationHelper {
	private static final int ARROW_SIZE = 2;
	private static final String CARDINALITY_ONE = "1";
	private static final String CARDINALITY_ZERO_OR_ONE = "0..1";
	private static final String CARDINALITY_ZERO_OR_MANY = "0..*";
	private static final int HORIZONTAL_DISTANCE_COMPOSITION = 30;
	private static final int HORIZONTAL_DISTANCE_STANDARD = 20;
	private static final int INHERITANCE_SIZE = 3;
	private static final int VERTICAL_DISTANCE = 5;

	/**
	 * Prevent instantiation
	 */
	private DomainAssociationDecorationHelper() {

	}

	/**
	 * @return the decoration for a composition
	 */
	public static RotatableDecoration createSourceCompositionDecoration() {
		return createSourceDecoration(true);
	}

	/**
	 * @return the decoration for an aggregation
	 */
	public static RotatableDecoration createSourceAggregationDecoration() {
		return createSourceDecoration(false);
	}

	/**
	 * @return the decoration for the domain object inheritance
	 */
	public static RotatableDecoration createInheritanceDecoration() {
		final PointList points = new PointList();
		points.addPoint(INHERITANCE_SIZE * -1, INHERITANCE_SIZE);
		points.addPoint(0, 0);
		points.addPoint(INHERITANCE_SIZE * -1, INHERITANCE_SIZE * -1);

		final PolygonDecoration decoration = new PolygonDecoration();
		decoration.setTemplate(points);
		decoration.setBackgroundColor(ColorConstants.white);

		return decoration;
	}

	/**
	 * @return the decoration for a unidirectional association target
	 */
	public static RotatableDecoration createTargetArrowDecoration() {
		final PointList points = new PointList();
		points.addPoint(ARROW_SIZE * -1, ARROW_SIZE);
		points.addPoint(0, 0);
		points.addPoint(ARROW_SIZE * -1, ARROW_SIZE * -1);

		final var decoration = new PolylineDecoration();
		decoration.setTemplate(points);

		return decoration;
	}

	/**
	 * Add the labels to the connection that represents an enum association
	 * @param connection the connection to add the labels to
	 * @param association the enum association
	 */
	public static void addLabelsToConnection(PolylineConnection connection, EnumAssociation association) {
		final var targetLocatorName = new ConnectionEndpointLocator(connection, true);
		targetLocatorName.setVDistance(-VERTICAL_DISTANCE);
		targetLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetName = new Label(association.getDomainAttribute().getName());
		targetName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetName, targetLocatorName);
	}

	/**
	 * Add the labels to the connection that represents a many-to-one association
	 * @param connection the connection to add the labels to
	 * @param association the enum association
	 */
	public static void addLabelsToConnection(PolylineConnection connection, ManyToOneAssociation association) {
		final var targetLocatorCardinality = new ConnectionEndpointLocator(connection, true);
		targetLocatorCardinality.setVDistance(VERTICAL_DISTANCE);
		targetLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetCardinality = new Label(CARDINALITY_ONE);
		targetCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		if (association.isOptional())
			targetCardinality.setText(CARDINALITY_ZERO_OR_ONE);

		connection.add(targetCardinality, targetLocatorCardinality);

		final var targetLocatorName = new ConnectionEndpointLocator(connection, true);
		targetLocatorName.setVDistance(-VERTICAL_DISTANCE);
		targetLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetName = new Label(association.getName());
		targetName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetName, targetLocatorName);
	}

	/**
	 * Add the labels to the connection that represents a one-to-many association
	 * @param connection the connection to add the labels to
	 * @param association the one-to-many association
	 */
	public static void addLabelsToConnection(PolylineConnection connection, OneToManyAssociation association) {
		if (association.isBidirectional()) {
			final var sourceLocatorCardinality = new ConnectionEndpointLocator(connection, false);
			sourceLocatorCardinality.setVDistance(VERTICAL_DISTANCE);
			sourceLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_COMPOSITION);

			final var sourceCardinality = new Label(CARDINALITY_ONE);
			sourceCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

			if (association.getReverseAssociation().isOptional())
				sourceCardinality.setText(CARDINALITY_ZERO_OR_ONE);

			connection.add(sourceCardinality, sourceLocatorCardinality);

			final var sourceLocatorName = new ConnectionEndpointLocator(connection, false);
			sourceLocatorName.setVDistance(-VERTICAL_DISTANCE);
			sourceLocatorName.setUDistance(HORIZONTAL_DISTANCE_COMPOSITION);

			final var sourceName = new Label(association.getReverseAssociation().getName());
			sourceName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

			connection.add(sourceName, sourceLocatorName);
		}

		final var targetLocatorCardinality = new ConnectionEndpointLocator(connection, true);
		targetLocatorCardinality.setVDistance(VERTICAL_DISTANCE);
		targetLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetCardinality = new Label(CARDINALITY_ZERO_OR_MANY);
		targetCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetCardinality, targetLocatorCardinality);

		final var targetLocatorName = new ConnectionEndpointLocator(connection, true);
		targetLocatorName.setVDistance(-VERTICAL_DISTANCE);
		targetLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetName = new Label(association.getName());
		targetName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetName, targetLocatorName);
	}

	/**
	 * Add the labels to the connection that represents a one-to-one association
	 * @param connection the connection to add the labels to
	 * @param association the one-to-one association
	 */
	public static void addLabelsToConnection(PolylineConnection connection, OneToOneAssociation association) {
		if (association.isBidirectional()) {
			final var sourceLocatorCardinality = new ConnectionEndpointLocator(connection, false);
			sourceLocatorCardinality.setVDistance(VERTICAL_DISTANCE);

			if (association.isOptional())
				sourceLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_STANDARD);
			else
				sourceLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_COMPOSITION);

			final var sourceCardinality = new Label(CARDINALITY_ONE);
			sourceCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

			if (association.getReverseAssociation().isOptional())
				sourceCardinality.setText(CARDINALITY_ZERO_OR_ONE);

			connection.add(sourceCardinality, sourceLocatorCardinality);

			final var sourceLocatorName = new ConnectionEndpointLocator(connection, false);
			sourceLocatorName.setVDistance(-VERTICAL_DISTANCE);

			if (association.isOptional())
				sourceLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);
			else
				sourceLocatorName.setUDistance(HORIZONTAL_DISTANCE_COMPOSITION);

			final var sourceName = new Label(association.getReverseAssociation().getName());
			sourceName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

			connection.add(sourceName, sourceLocatorName);
		}

		final var targetLocatorCardinality = new ConnectionEndpointLocator(connection, true);
		targetLocatorCardinality.setVDistance(VERTICAL_DISTANCE);
		targetLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetCardinality = new Label(CARDINALITY_ONE);
		targetCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		if (association.isOptional())
			targetCardinality.setText(CARDINALITY_ZERO_OR_ONE);

		connection.add(targetCardinality, targetLocatorCardinality);

		final var targetLocatorName = new ConnectionEndpointLocator(connection, true);
		targetLocatorName.setVDistance(-VERTICAL_DISTANCE);
		targetLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetName = new Label(association.getName());
		targetName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetName, targetLocatorName);
	}

	/**
	 * Add the labels to the connection that represents a many-to-many association
	 * @param connection the connection to add the labels to
	 * @param association the many-to-many association
	 */
	public static void addLabelsToConnection(PolylineConnection connection, ManyToManyAssociation association) {
		if (association.isBidirectional()) {
			final var sourceLocatorCardinality = new ConnectionEndpointLocator(connection, false);
			sourceLocatorCardinality.setVDistance(VERTICAL_DISTANCE);
			sourceLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

			final var sourceCardinality = new Label(CARDINALITY_ZERO_OR_MANY);
			sourceCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

			connection.add(sourceCardinality, sourceLocatorCardinality);

			final var sourceLocatorName = new ConnectionEndpointLocator(connection, false);
			sourceLocatorName.setVDistance(-VERTICAL_DISTANCE);
			sourceLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

			final var sourceName = new Label(association.getReverseAssociation().getName());
			sourceName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

			connection.add(sourceName, sourceLocatorName);
		}

		final var targetLocatorCardinality = new ConnectionEndpointLocator(connection, true);
		targetLocatorCardinality.setVDistance(VERTICAL_DISTANCE);
		targetLocatorCardinality.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetCardinality = new Label(CARDINALITY_ZERO_OR_MANY);
		targetCardinality.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetCardinality, targetLocatorCardinality);

		final var targetLocatorName = new ConnectionEndpointLocator(connection, true);
		targetLocatorName.setVDistance(-VERTICAL_DISTANCE);
		targetLocatorName.setUDistance(HORIZONTAL_DISTANCE_STANDARD);

		final var targetName = new Label(association.getName());
		targetName.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		connection.add(targetName, targetLocatorName);
	}

	/**
	 * @param isComposition flag that controls if the decoration is for a composition
	 * @return the source decoration
	 */
	private static PolygonDecoration createSourceDecoration(boolean isComposition) {
		final PointList points = new PointList();
		points.addPoint(0, 0);
		points.addPoint(ARROW_SIZE * -1, ARROW_SIZE);
		points.addPoint(ARROW_SIZE * -2, 0);
		points.addPoint(ARROW_SIZE * -1, ARROW_SIZE * -1);

		final PolygonDecoration decoration = new PolygonDecoration();
		decoration.setTemplate(points);

		if (isComposition)
			decoration.setBackgroundColor(ColorConstants.black);
		else
			decoration.setBackgroundColor(ColorConstants.white);

		return decoration;
	}

}
