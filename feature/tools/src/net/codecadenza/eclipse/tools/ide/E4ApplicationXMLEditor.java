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
package net.codecadenza.eclipse.tools.ide;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_PART_STACK_ID;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_PLUGIN_ID;

import java.io.StringReader;
import java.io.StringWriter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

/**
 * <p>
 * Helper class to change the content of application model files (*.e4xmi) when either creating or deleting views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class E4ApplicationXMLEditor {
	private static final short DEFAULT_ID_LENGTH = 22;
	private static final String ID_PREFIX = "_";
	private static final String ELEMENT_NAME_CHILDREN = "children";
	private static final String ATTR_NAME_TYPE = "xsi:type";
	private static final String ATTR_NAME_ID = "xmi:id";
	private static final String ATTR_NAME_ELEM_ID = "elementId";
	private static final String ATTR_NAME_CONTR_URI = "contributionURI";
	private static final String ATTR_NAME_CLOSEABLE = "closeable";
	private static final String ATTR_NAME_LABEL = "label";
	private static final String ATTR_NAME_VISIBLE = "visible";
	private static final String PART_TYPE = "basic:Part";
	private static final String PART_STACK_TYPE = "basic:PartStack";

	private final String content;

	/**
	 * Constructor
	 * @param content
	 */
	public E4ApplicationXMLEditor(String content) {
		this.content = content;
	}

	/**
	 * Add a view to the application model
	 * @param className
	 * @param label
	 * @return the new content of the application model file
	 * @throws Exception if the remove operation has failed
	 */
	public String addView(String className, String label) throws Exception {
		try {
			final Document doc = parseContent();

			addPartElement(doc, className, label);

			return convertToString(doc);
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);

			// Re-throw the original exception
			throw e;
		}
	}

	/**
	 * Remove a view from the application model
	 * @param className
	 * @return the new content of the application model file
	 * @throws Exception if the remove operation has failed
	 */
	public String removeView(String className) throws Exception {
		try {
			final Document doc = parseContent();

			removePartElement(doc, className);

			return convertToString(doc);
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);

			// Re-throw the original exception
			throw e;
		}
	}

	/**
	 * Parse the content
	 * @return the DOM document
	 * @throws IllegalArgumentException if no content is available
	 * @throws Exception if parsing has failed
	 */
	private Document parseContent() throws Exception {
		final DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance();
		final DocumentBuilder parser = fac.newDocumentBuilder();

		if (content == null)
			throw new IllegalArgumentException("The content must not be null!");

		final var input = new InputSource(new StringReader(content));

		return parser.parse(input);
	}

	/**
	 * Get the part stack node that is the parent for all generated views
	 * @param doc
	 * @return the part stack node or null if a respective node could not be found
	 */
	private Node getPartStackNode(Document doc) {
		final NodeList children = doc.getElementsByTagName(ELEMENT_NAME_CHILDREN);
		Node partStackNode = null;

		for (int i = 0; i < children.getLength(); i++) {
			final NamedNodeMap attrMap = children.item(i).getAttributes();

			if (attrMap == null)
				continue;

			if (attrMap.getNamedItem(ATTR_NAME_TYPE) != null) {
				final Node typeNode = attrMap.getNamedItem(ATTR_NAME_TYPE);

				if (typeNode.getTextContent() != null && typeNode.getTextContent().equals(PART_STACK_TYPE)) {
					// Save the current part stack in order to return it later if the default part stack cannot be found!
					partStackNode = children.item(i);

					if (attrMap.getNamedItem(ATTR_NAME_ELEM_ID) != null) {
						final Node elementIdNode = attrMap.getNamedItem(ATTR_NAME_ELEM_ID);

						if (elementIdNode.getTextContent() != null && elementIdNode.getTextContent().equals(DEFAULT_PART_STACK_ID))
							return partStackNode;
					}
				}
			}
		}

		return partStackNode;
	}

	/**
	 * Convert the DOM representation of the application model to a string
	 * @param doc
	 * @return the given document as a string
	 * @throws Exception if the conversion has failed
	 */
	private String convertToString(Document doc) throws Exception {
		final var domSource = new DOMSource(doc);
		final var writer = new StringWriter();
		final var result = new StreamResult(writer);

		final TransformerFactory tf = TransformerFactory.newInstance();
		tf.setAttribute("indent-number", 3);

		final Transformer transformer = tf.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.setOutputProperty(OutputKeys.METHOD, "html");
		transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "application/xml");
		transformer.transform(domSource, result);

		return writer.toString();
	}

	/**
	 * Remove a part element that represents a view from the document
	 * @param doc
	 * @param className the fully qualified name of the view
	 */
	private void removePartElement(Document doc, String className) {
		// Search for the parent of all views
		final Node partStackNode = getPartStackNode(doc);

		if (partStackNode == null)
			throw new IllegalStateException(
					"The view could not be added to the application model as the respective part stack could not be found!");

		final NodeList children = partStackNode.getChildNodes();

		for (int i = 0; i < children.getLength(); i++) {
			final NamedNodeMap attrMap = children.item(i).getAttributes();

			if (attrMap == null)
				continue;

			if (attrMap.getNamedItem(ATTR_NAME_TYPE) != null && attrMap.getNamedItem(ATTR_NAME_ELEM_ID) != null) {
				final Node typeNode = attrMap.getNamedItem(ATTR_NAME_TYPE);
				final Node elementIdNode = attrMap.getNamedItem(ATTR_NAME_ELEM_ID);

				if (typeNode.getTextContent().equals(PART_TYPE) && elementIdNode.getTextContent().equals(className)) {
					partStackNode.removeChild(children.item(i));
					return;
				}
			}
		}
	}

	/**
	 * Add a part to the part stack
	 * @param doc
	 * @param className the fully qualified name of the view
	 * @param label
	 */
	private void addPartElement(Document doc, String className, String label) {
		final Node partStackNode = getPartStackNode(doc);

		if (partStackNode == null)
			throw new IllegalStateException(
					"The view could not be added to the application model as the respective part stack could not be found!");

		final Element elem = doc.createElement(ELEMENT_NAME_CHILDREN);

		final var generator = new IDGenerator();
		generator.setLength(DEFAULT_ID_LENGTH);
		generator.setPrefix(ID_PREFIX);

		elem.setAttribute(ATTR_NAME_ID, generator.generateNextID());
		elem.setAttribute(ATTR_NAME_ELEM_ID, className);
		elem.setAttribute(ATTR_NAME_TYPE, PART_TYPE);
		elem.setAttribute(ATTR_NAME_CONTR_URI, "bundleclass://" + DEFAULT_PLUGIN_ID + "/" + className);
		elem.setAttribute(ATTR_NAME_CLOSEABLE, Boolean.toString(true));
		elem.setAttribute(ATTR_NAME_LABEL, label);
		elem.setAttribute(ATTR_NAME_VISIBLE, Boolean.toString(false));

		partStackNode.appendChild(elem);
	}

}
