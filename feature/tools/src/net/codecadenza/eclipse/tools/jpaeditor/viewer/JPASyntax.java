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
package net.codecadenza.eclipse.tools.jpaeditor.viewer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Data object for the JPA syntax
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPASyntax {
	private static final String[] RESERVED_WORDS = { "select", "from", "where", "update", "delete", "join", "outer", "inner",
			"left", "group", "by", "having", "fetch", "distinct", "object", "null", "true", "false", "not", "and", "or", "between",
			"like", "in", "as", "unknown", "empty", "member", "of", "is", "avg", "max", "min", "sum", "count", "order", "asc", "desc",
			"mod", "upper", "lower", "trim", "position", "character_length", "char_length", "bit_length", "current_time",
			"current_date", "current_timestamp", "new", "exists", "all", "any", "some" };

	private final HashSet<String> reservedWords = new HashSet<>();
	private final HashMap<String, ArrayList<String>> domainObjectAttributeMap = new HashMap<>();
	private final HashMap<String, ArrayList<String>> domainObjectAssocMap = new HashMap<>();

	/**
	 * Constructor
	 * @param project
	 */
	public JPASyntax(Project project) {
		reservedWords.addAll(Stream.of(RESERVED_WORDS).toList());

		final Collection<DomainObject> domainObjects = project.getAllDomainObjectsOfProject(false, true);

		domainObjects.forEach(b -> {
			final var attributes = new ArrayList<String>();

			b.getAllAttributes().forEach(attr -> attributes.add(attr.getName()));

			domainObjectAttributeMap.put(b.getName(), attributes);

			final var assocs = new ArrayList<String>();

			b.getAllAssociations().forEach(assoc -> assocs.add(assoc.getName()));

			domainObjectAssocMap.put(b.getName(), assocs);
		});
	}

	/**
	 * @return the reserved words
	 */
	public Set<String> getReservedWords() {
		return reservedWords;
	}

	/**
	 * @return a map containing all domain objects including all attributes
	 */
	public Map<String, ArrayList<String>> getDomainObjectAttributeMap() {
		return domainObjectAttributeMap;
	}

	/**
	 * @return a map containing all domain objects including all associations
	 */
	public Map<String, ArrayList<String>> getDomainAssociationMap() {
		return domainObjectAssocMap;
	}

	/**
	 * @return all words including reserved words, persistent objects and attribute names
	 */
	public Set<String> getAllWords() {
		final var allWords = new HashSet<>(reservedWords);
		allWords.addAll(domainObjectAttributeMap.keySet());

		domainObjectAttributeMap.keySet().forEach(domainObject -> allWords.addAll(domainObjectAttributeMap.get(domainObject)));

		return allWords;
	}

}
