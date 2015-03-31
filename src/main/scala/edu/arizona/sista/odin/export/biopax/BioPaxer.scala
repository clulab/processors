package edu.arizona.sista.odin.export.biopax

import java.io._

import scala.collection.mutable.MutableList
import scala.collection.mutable.Map

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._

import org.biopax.paxtools.io._
import org.biopax.paxtools.model._
import org.biopax.paxtools.model.level3._

/**
  * Defines implicit classes used to build and output BioPax models.
  *   Written by Tom Hicks. 3/6/2015.
  *   Last Modified: Add filter to output of string mentions.
  */
class BioPaxer {
  // Type aliases:
  type Gene = Protein                       // temporary until we decide how to handle genes
  type GeneReference = ProteinReference     // temporary until we decide how to handle genes

  // Constants:
  val SistaBaseURL = "http://nlp.sista.arizona.edu/odin/"
  val SistaDefaultCharset = "UTF-8"

  // external knowledge base accessors
  protected val cellCompKB = new GeneOntologyKBAccessor
  protected val geneKB = new GeneOntologyKBAccessor
  protected val proteinKB = new UniprotKBAccessor
  protected val smallMoleculeKB = new ChEBIKBAccessor

  // incrementing ID for numbering entities
  protected val idCntr = new IncrementingId()

  // local caches for physical entities: map entity type names to entity reference subclasses
  protected val cellCompRefs = scala.collection.mutable.Map[String, ControlledVocabulary]()
  protected val geneRefs = scala.collection.mutable.Map[String, GeneReference]()
  protected val proteinRefs = scala.collection.mutable.Map[String, ProteinReference]()
  protected val smallMoleculeRefs = scala.collection.mutable.Map[String, SmallMoleculeReference]()


  /** Build and return a BioPax model for the given sequence of mentions. */
  def buildModel (mentions:Seq[Mention], doc:Document): Model = {
    // create and initialize a new BioPAX model:
    val factory: BioPAXFactory = BioPAXLevel.L3.getDefaultFactory()
    var model:Model = factory.createModel()
//    model.setXmlBase(SistaBaseURL)          // TODO: UNCOMMENT LATER?

    // TODO: Set XML namespace for SISTA NLP UAZ?
    // TODO: Add dataSource (Provenance) information for SISTA NLP UAZ

    addPublicationXref(model, doc)          // add publication xref for the current document

    mentions.filter(_.matches("Event")).foreach {
      addMention(model, _)
    }
    return model
  }

  /** Add the given mention to the given model and return the model. */
  def addMention (model:Model, mention: Mention): Model = {
    // TODO: parse and accumulate arguments in structure
    mention match {
      case mention: TextBoundMention =>
        addTextBoundMention(model, mention)
      case mention: EventMention =>
        addEventMention(model, mention)
      case mention: RelationMention =>
        addRelationMention(model, mention)
      case _ => ()
    }
    return model                            // return updated model
  }

  def addEventMention (model:Model, mention:EventMention) = {
    mention.label match {                   // dispatch on mention type
      case "Binding" =>
      case "Degradation" =>
      case "Exchange" =>
      case "Hydrolysis" =>
      case "Hydroxylation" =>
//      case "Phosphorylation" => doPhosphorylation(model, mention)
      case "Positive_regulation" =>
      case "Transcription" =>
      case "Transport" =>
      case "Ubiquitination" =>
      case _ => ()
    }
  }

  def addRelationMention (model:Model, mention:Mention) = {
    mention.label match {                   // dispatch on mention type
      case "Negative_regulation" =>
      case "Protein_with_site" =>
      case "Positive_regulation" =>
      case "Regulation" =>
      case "Ubiquitination" =>
      case _ => ()
    }
  }

  def addTextBoundMention (model:Model, mention:Mention): Entity = {
    mention.label match {
      // TODO: need to restore this:
//      case "Cellular_component" => return doCellularComponent(model, mention)
      case "Gene_or_gene_product" => return doGene(model, mention)
      case "Protein" => return doProtein(model, mention)
      case "Simple_chemical" => return doSmallMolecule(model, mention)
      case "Site" => return null
      case _ => return null
    }
  }

  def doPhosphorylation (model:Model, mention:Mention): BiochemicalReaction = {
    val themes = getTheme(mention)
    if (themes != null) {
      val theme = themes(0)
      val left = addTextBoundMention(model, theme)
      val right = addTextBoundMention(model, theme)
    }
    return null
  }


  /** Return a vocabulary item for the cellular component represented by the given mention. */
  def doCellularComponent (model:Model, mention:Mention): ControlledVocabulary = {
    // cellular location is a property not an entity: must be attached to an entity (in future)
    return referenceForCellularComponent(model, mention)
  }

  /** Create gene instance and entity reference, add them to the model, and return instance. */
  def doGene (model:Model, mention:Mention): Gene = {
    val gRef:GeneReference = referenceForGene(model, mention)
    return addGeneToModel(model, mention, gRef)
  }

  /** Create protein instance and entity reference, add them to the model, and return instance. */
  def doProtein (model:Model, mention:Mention): Protein = {
    val pRef:ProteinReference = referenceForProtein(model, mention)
    return addProteinToModel(model, mention, pRef)
  }

  /** Create small molecule instance and entity reference, add them to the model, and return instance. */
  def doSmallMolecule (model:Model, mention:Mention): SmallMolecule = {
    val smRef:SmallMoleculeReference = referenceForSmallMolecule(model, mention)
    return addSmallMoleculeToModel(model, mention, smRef)
  }


  /** Add a gene instance and associated entity reference to the model. */
  private def addGeneToModel (model:Model, mention:Mention, gRef:GeneReference): Gene = {
    val name = geneKB.getLookupKey(mention)
    val gUrl = genInternalURL(s"G_${idCntr.genNextId()}")
    val gene:Gene = model.addNew(classOf[Gene], gUrl)
    gene.setDisplayName(name)
    gene.setEntityReference(gRef)
    return gene
  }

  /** Add a protein instance and associated entity reference to the model. */
  private def addProteinToModel (model:Model, mention:Mention, pRef:ProteinReference): Protein = {
    val name = proteinKB.getLookupKey(mention)
    val pUrl = genInternalURL(s"P_${idCntr.genNextId()}")
    val prot:Protein = model.addNew(classOf[Protein], pUrl)
    prot.setDisplayName(name)
    prot.setEntityReference(pRef)
    return prot
  }

  /** Add a small molecule instance and associated entity reference to the model. */
  private def addSmallMoleculeToModel (model:Model, mention:Mention,
                                       eRef:SmallMoleculeReference): SmallMolecule = {
    val name = smallMoleculeKB.getLookupKey(mention)
    val eUrl = genInternalURL(s"SM_${idCntr.genNextId()}")
    val sMole:SmallMolecule = model.addNew(classOf[SmallMolecule], eUrl)
    sMole.setDisplayName(name)
    sMole.setEntityReference(eRef)
    return sMole
  }

  /** Add a PublicationXref for the given document and save it as a class variable. */
  private def addPublicationXref (model:Model, doc:Document) = {
    val pxrUrl = s"${SistaBaseURL}PX_${idCntr.genNextId()}"
    model.addNew(classOf[PublicationXref], pxrUrl)
  }

  /** Generate an internal URL for the given ID string. */
  private def genInternalURL (id:String): String = {
    return s"${SistaBaseURL}${id}"
  }

  /** Generate a unification Xref with the given arguments. */
  private def genUnificationXref (model:Model, eId:String, eNamespace:String): UnificationXref = {
    val xUrl = genInternalURL(s"UX_${idCntr.genNextId()}")
    val uXref:UnificationXref = model.addNew(classOf[UnificationXref], xUrl)
    uXref.setId(eId)
    uXref.setDb(eNamespace)
    return uXref
  }

  private def getTheme (mention:Mention): Seq[Mention] = {
    mention.arguments.foreach {
      case (k, vs) => if (k startsWith "theme") return vs
    }
    return Nil
  }

  /** Lookup or create a controlled vocabulary item for the given cellular component mention. */
  private def referenceForCellularComponent (model:Model, mention:Mention): ControlledVocabulary = {
    val name = cellCompKB.getLookupKey(mention)
    return cellCompRefs.getOrElseUpdate(name, registerCellularLocation(model, mention))
  }

  /** Lookup or create a reference for the given gene mention. */
  private def referenceForGene (model:Model, mention:Mention): GeneReference = {
    val name = geneKB.getLookupKey(mention)
    return geneRefs.getOrElseUpdate(name, registerGene(model, mention))
  }

  /** Lookup or create a reference for the given protein mention. */
  private def referenceForProtein (model:Model, mention:Mention): ProteinReference = {
    val name = proteinKB.getLookupKey(mention)
    return proteinRefs.getOrElseUpdate(name, registerProtein(model, mention))
  }

  /** Lookup or create a reference for the given small molecule mention. */
  private def referenceForSmallMolecule (model:Model, mention:Mention): SmallMoleculeReference = {
    val name = smallMoleculeKB.getLookupKey(mention)
    return smallMoleculeRefs.getOrElseUpdate(name, registerSmallMolecule(model, mention))
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerCellularLocation (model:Model, mention:Mention): CellularLocationVocabulary = {
    // TODO: error handling for the lookup:
    val eInfo = cellCompKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = s"CLV_GO:${idCntr.genNextId()}"  // MOCK missing referenceID value
    val eUrl = cellCompKB.referenceURI(eId)  // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:CellularLocationVocabulary = model.addNew(classOf[CellularLocationVocabulary], eUrl)
    eRef.addTerm(eName)
    eRef.addXref(uXref)

    cellCompRefs.put(eName, eRef)           // memoize new reference
    return eRef                             // and return it
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerGene (model:Model, mention:Mention): GeneReference = {
    // TODO: error handling for the lookup:
    val eInfo = geneKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = s"rG_${idCntr.genNextId()}"   // MOCK missing referenceID value
    val eUrl = geneKB.referenceURI(eId)     // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:GeneReference = model.addNew(classOf[GeneReference], eUrl)
    eRef.setDisplayName(eName)
    eRef.addXref(uXref)

    geneRefs.put(eName, eRef)               // memoize new reference
    return eRef                             // and return it
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerProtein (model:Model, mention:Mention): ProteinReference = {
    // TODO: error handling for the lookup:
    val eInfo = proteinKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = s"rP_${idCntr.genNextId()}"   // MOCK missing referenceID value
    val eUrl = proteinKB.referenceURI(eId)  // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:ProteinReference = model.addNew(classOf[ProteinReference], eUrl)
    eRef.setDisplayName(eName)
    eRef.addXref(uXref)

    proteinRefs.put(eName, eRef)            // memoize new reference
    return eRef                             // and return it
  }

  /** Create and return a new entity reference from the given mention, memoizing it
    * locally, as a side effect. */
  private def registerSmallMolecule (model:Model, mention:Mention): SmallMoleculeReference = {
    // TODO: error handling for the lookup:
    val eInfo = smallMoleculeKB.resolve(mention) // resolve mention to info about physical entity
    val eName = eInfo("key")
    // val eId = eInfo("referenceID")
    // val eUrl = eInfo("referenceURI")

    // Temporary code until we do resolution against the real KB:
    val eId = s"rSM_${idCntr.genNextId()}"  // MOCK missing referenceID value
    val eUrl = smallMoleculeKB.referenceURI(eId) // MOCK value with manual call

    val uXref = genUnificationXref(model, eId, eInfo("namespace"))
    val eRef:SmallMoleculeReference = model.addNew(classOf[SmallMoleculeReference], eUrl)
    eRef.setDisplayName(eName)
    eRef.addXref(uXref)

    smallMoleculeRefs.put(eName, eRef)      // memoize new reference
    return eRef                             // and return it
  }


  /** Generates a BioPax representation of the given mention as a list of strings. */
  def mentionToStrings (mention: Mention): List[String] = {
    return mentionToStrings(mention, 0)
  }

  private def mentionToStrings (mention: Mention, level:Integer): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    val indent = ("  " * level)
    mention match {
      case mention: TextBoundMention =>
        mStrings += s"${indent}TextBoundMention: (S${mention.sentence}): ${mention.label}"
        mStrings += s"${indent}text: ${mention.text}"
        if (level == 0) mStrings += ("=" * 80)
      case mention: EventMention =>
        mStrings += s"${indent}EventMention: (S${mention.sentence}): ${mention.label}"
        mStrings += s"${indent}text: ${mention.text}"
        mStrings += s"${indent}trigger:"
        mStrings ++= mentionToStrings(mention.trigger, level+1)
        mention.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k}:"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }
        if (level == 0) mStrings += ("=" * 80)
      case mention: RelationMention =>
        mStrings += s"${indent}RelationMention: (S${mention.sentence}): ${mention.label}"
        mStrings += s"${indent}text: ${mention.text}"
        mention.arguments foreach {
          case (k,vs) => {
            mStrings += s"${indent}${k}:"
            for (v <- vs) {
              mStrings ++= mentionToStrings(v, level+1)
            }
          }
        }
        if (level == 0) mStrings += ("=" * 80)
      case _ => ()
    }
    return mStrings.toList
  }


  /** return the given model as a single BioPax OWL string. */
  def modelToString (model:Model): String = {
    val bpIOH:BioPAXIOHandler = new SimpleIOHandler()
    val baos:ByteArrayOutputStream = new ByteArrayOutputStream()
    bpIOH.convertToOWL(model, baos)
    return baos.toString(SistaDefaultCharset)
  }

  /** Output the given model to the given output stream. */
  def outputModel (model:Model, out:OutputStream): Unit = {
    val bpIOH:BioPAXIOHandler = new SimpleIOHandler()
    bpIOH.convertToOWL(model, out)
  }

  /** Output a string representation of the mentions selected by the given label string
    * to the given output stream.
    * NB: This method closes the given output stream when done!
    */
  def outputFilteredMentions (mentionType:String,
                              mentions:Seq[Mention],
                              doc:Document,
                              fos:FileOutputStream): Unit =
  {
    val out:PrintWriter = new PrintWriter(new BufferedWriter(new OutputStreamWriter(fos)))
    mentions.filter(_.matches(mentionType)).foreach { mention =>
      mentionToStrings(mention).foreach { str => out.println(str) }
    }
    out.flush()
    out.close()
  }

}


/** Implements an incrementing identification string for numbering entities. */
class IncrementingId {
  protected var cntr = 0

  /** Return the current identification string. */
  def currentId():String = { s"${cntr}" }

  /** Increment counter and return new identification string. */
  def genNextId():String = {
    cntr = cntr + 1
    return currentId()
  }
}
